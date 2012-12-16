module Events.EventTree (
     DurationTree, DurationNode(..),
     mkDurationTree,

     runTimeOf, gcTimeOf,
     reportDurationTree,
     durationTreeCountNodes,
     durationTreeMaxDepth,

     EventTree(..), EventNode(..),
     mkEventTree,
     reportEventTree, eventTreeMaxDepth,
  ) where

import Events.TimeTree
import Events.EventDuration

import qualified GHC.RTS.Events as GHC
import GHC.RTS.Events hiding (Event)

import Text.Printf
import Control.Exception (assert)
import Data.Monoid

-------------------------------------------------------------------------------

-- We map the events onto a binary search tree, so that we can easily
-- find the events that correspond to a particular view of the
-- timeline.  Additionally, each node of the tree contains a summary
-- of the information below it, so that we can render views at various
-- levels of resolution.  For example, if a tree node would represent
-- less than one pixel on the display, there is no point is descending
-- the tree further.

-- We only split at event boundaries; we never split an event into
-- multiple pieces.  Therefore, the binary tree is only roughly split
-- by time, the actual split depends on the distribution of events
-- below it.

data DurationNode
  = OverviewNode
        {-#UNPACK#-}!Timestamp -- The total amount of time spent running a thread
        {-#UNPACK#-}!Timestamp -- The total amount of time spend in GC
  | DurationNode
        EventDuration

instance Monoid DurationNode where
  mempty        = OverviewNode 0 0
  n `mappend` m = OverviewNode (runTimeOf n + runTimeOf m) (gcTimeOf n + gcTimeOf m)

instance Show DurationNode where
  show n = "Run " ++ show (runTimeOf n) ++ " ms, GC " ++ show (gcTimeOf n) ++ " ms"

type DurationTree = TimeTree DurationNode

-------------------------------------------------------------------------------

mkDurationTree :: [EventDuration] -> Timestamp -> DurationTree
mkDurationTree es endTime = mkTimeTree nodes endTime
  where nodes = map stamp $ filter ((>0) . durationOf) es
        stamp d = (startTimeOf d, DurationNode d)
        -- PMW: Filter out all zero-length nodes. I suppose they wouldn't
        --      have any effect either way?

-------------------------------------------------------------------------------

runTimeOf :: DurationNode -> Timestamp
runTimeOf (OverviewNode runTime _) = runTime
runTimeOf (DurationNode e) | ThreadRun{} <- e = durationOf e
runTimeOf _ = 0

-------------------------------------------------------------------------------

gcTimeOf :: DurationNode -> Timestamp
gcTimeOf (OverviewNode _ gcTime) = gcTime
gcTimeOf (DurationNode e) | isGCDuration e = durationOf e
gcTimeOf _ = 0

-------------------------------------------------------------------------------

reportDurationTree :: Int -> DurationTree -> IO ()
reportDurationTree hecNumber eventTree
  = putStrLn ("HEC " ++ show hecNumber ++ reportText)
    where
    reportText = " nodes = " ++ show (durationTreeCountNodes eventTree) ++
                 " max depth = " ++ show (durationTreeMaxDepth eventTree)

-------------------------------------------------------------------------------

durationTreeCountNodes :: DurationTree -> Int
durationTreeCountNodes = timeTreeSize

-------------------------------------------------------------------------------

durationTreeMaxDepth :: DurationTree -> Int
durationTreeMaxDepth = timeTreeMaxDepth

-------------------------------------------------------------------------------

data EventTree
    = EventTree
        {-#UNPACK#-}!Timestamp -- The start time of this run-span
        {-#UNPACK#-}!Timestamp -- The end   time of this run-span
        EventNode

data EventNode
  = EventSplit
        {-#UNPACK#-}!Timestamp -- The time used to split the events into two parts
        EventNode -- The LHS split; all events lie completely between
                  -- start and split
        EventNode -- The RHS split; all events lie completely between
                  -- split and end

  | EventTreeLeaf [GHC.Event]
        -- sometimes events happen "simultaneously" (at the same time
        -- given the resolution of our clock source), so we can't
        -- separate them.

  | EventTreeOne GHC.Event
        -- This is a space optimisation for the common case of
        -- EventTreeLeaf [e].

mkEventTree :: [GHC.Event] -> Timestamp -> EventTree
mkEventTree es endTime =
  EventTree s e $
  -- trace (show tree) $
  tree
 where
  tree = splitEvents es endTime
  (s,e) = if null es then (0,0) else (time (head es), endTime)

splitEvents :: [GHC.Event] -- events
            -> Timestamp       -- end time of last event in the list
            -> EventNode
splitEvents []  !_endTime =
  -- if len /= 0 then error "splitEvents0" else
  EventTreeLeaf []   -- The case for an empty list of events

splitEvents [e] !_endTime =
  EventTreeOne e

splitEvents es !endTime
  | duration == 0
  = EventTreeLeaf es

  | null rhs
  = splitEvents es lhs_end

  | null lhs
  = error $
    printf "splitEvents: null lhs: len = %d, startTime = %d, endTime = %d\n"
      (length es) startTime endTime
    ++ '\n': show es

  | otherwise
  = -- trace (printf "len = %d, startTime = %d, endTime = %d, lhs_len = %d\n" len startTime endTime lhs_len) $
    assert (length lhs + length rhs == length es) $
    EventSplit (time (head rhs))
               ltree
               rtree
    where
    -- | Integer division, rounding up.
    divUp :: Timestamp -> Timestamp -> Timestamp
    divUp n k = (n + k - 1) `div` k
    startTime = time (head es)
    splitTime = startTime + (endTime - startTime) `divUp` 2
    duration  = endTime - startTime

    (lhs, lhs_end, rhs) = splitEventList es [] splitTime 0

    ltree = splitEvents lhs lhs_end
    rtree = splitEvents rhs endTime


splitEventList :: [GHC.Event]
               -> [GHC.Event]
               -> Timestamp
               -> Timestamp
               -> ([GHC.Event], Timestamp, [GHC.Event])
splitEventList []  acc !_tsplit !tmax
  = (reverse acc, tmax, [])
splitEventList [e] acc !_tsplit !tmax
  -- Just one event left: put it on the right. This ensures that we
  -- have at least one event on each side of the split.
  = (reverse acc, tmax, [e])
splitEventList (e:es) acc !tsplit !tmax
  | t <= tsplit  -- pick all events that start at or before the split
  = splitEventList es (e:acc) tsplit (max tmax t)
  | otherwise
  = (reverse acc, tmax, e:es)
  where
    t = time e

-------------------------------------------------------------------------------

reportEventTree :: Int -> EventTree -> IO ()
reportEventTree hecNumber (EventTree _ _ eventTree)
  = putStrLn ("HEC " ++ show hecNumber ++ reportText)
    where
    reportText = " nodes = " ++ show (eventTreeCountNodes eventTree) ++
                 " max depth = " ++ show (eventNodeMaxDepth eventTree)

-------------------------------------------------------------------------------

eventTreeCountNodes :: EventNode -> Int
eventTreeCountNodes (EventSplit _ lhs rhs)
   = 1 + eventTreeCountNodes lhs + eventTreeCountNodes rhs
eventTreeCountNodes _ = 1

-------------------------------------------------------------------------------

eventTreeMaxDepth :: EventTree -> Int
eventTreeMaxDepth (EventTree _ _ t) = eventNodeMaxDepth t

eventNodeMaxDepth :: EventNode -> Int
eventNodeMaxDepth (EventSplit _ lhs rhs)
  = 1 + eventNodeMaxDepth lhs `max` eventNodeMaxDepth rhs
eventNodeMaxDepth _ = 1
