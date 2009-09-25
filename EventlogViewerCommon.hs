module EventlogViewerCommon
where
import Data.IORef
import Data.List

-- import Debug.Trace
-- import Text.Printf

import qualified GHC.RTS.Events as GHCEvents
import GHC.RTS.Events hiding (Event)

-------------------------------------------------------------------------------
-- This datastructure is a duration-based representation of the event
-- loginformation where thread-runs and GCs are explicitly represented
-- by a single constructor identifying their start and end points.

data EventDuration
  = ThreadRun {-#UNPACK#-}!ThreadId 
              ThreadStopStatus
              {-#UNPACK#-}!Timestamp
              {-#UNPACK#-}!Timestamp

  | GC {-#UNPACK#-}!Timestamp
       {-#UNPACK#-}!Timestamp

  | EV GHCEvents.Event
  deriving Show

-------------------------------------------------------------------------------
-- The start time of an event.

timeOfEventDuration :: EventDuration -> Timestamp
timeOfEventDuration ed
  = case ed of
      ThreadRun _ _ startTime _ -> startTime
      GC startTime _            -> startTime
      EV event                  -> time event

-------------------------------------------------------------------------------
-- The emd time of an event.

endTimeOfEventDuration :: EventDuration -> Timestamp
endTimeOfEventDuration ed
  = case ed of
      ThreadRun _ _ _ endTime -> endTime
      GC _ endTime            -> endTime
      EV event                -> time event

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

data EventTree
  = EventSplit
        {-#UNPACK#-}!Timestamp -- The start time of this run-span
	{-#UNPACK#-}!Timestamp -- The time used to split the events into two parts
	{-#UNPACK#-}!Timestamp -- The end time of this run-span
	EventTree -- The LHS split; all events lie completely between
                  -- start and split
        EventTree -- The RHS split; all events lie completely between
	          -- split and end
        {-#UNPACK#-}!Int       -- The number of events under this node
        {-#UNPACK#-}!Timestamp -- The total amount of time spent running a thread
        {-#UNPACK#-}!Timestamp -- The total amount of time spend in GC

  | EventTreeLeaf
        [EventDuration]

  deriving Show

-------------------------------------------------------------------------------

splitEvents :: [EventDuration] -> EventTree
splitEvents es = 
  -- trace (show tree) $
  tree
 where
  tree = splitEvents' es (length es) (endTimeOfEventDuration (last es))

splitEvents' :: [EventDuration] -- events
             -> Int             -- length of list above
             -> Timestamp       -- end time of last event in the list
             -> EventTree
splitEvents' []  _len _endTime = 
  -- if len /= 0 then error "splitEvents'0" else
  EventTreeLeaf []   -- The case for an empty list of events

splitEvents' es   len endTime
  | duration == 0 || len < 2
  = -- if len /= length es then error (printf "splitEvents'3; %d %d" len (length es))  else 
    -- trace (printf "leaf: len = %d, startTime = %d\n" len startTime) $ 
    EventTreeLeaf es

  -- if we didn't manage to split any events from the left, then this
  -- event sequence must be unsplittable.  For example, if there is
  -- one event whose duration contains multiple instantaneous events,
  -- we cannot split it.
  | null lhs
  = EventTreeLeaf es

  | otherwise
  = -- trace (printf "len = %d, startTime = %d, endTime = %d, lhs_len = %d\n" len startTime endTime lhs_len) $
    -- if len /= length es || length lhs + length rhs /= len then error (printf "splitEvents'3; %d %d %d %d %d" len (length es) (length lhs) lhs_len (length rhs))  else 
    EventSplit startTime
	       lhs_end
               endTime 
               ltree
               rtree
               len -- Number of events under this node
               runTime
               gcTime
    where
    startTime = timeOfEventDuration (head es)
    splitTime = startTime + (endTime - startTime) `div` 2
    duration  = endTime - startTime

    (lhs, lhs_len, lhs_end, rhs) = splitEventList es [] splitTime 0 endTime 0

    ltree = splitEvents' lhs lhs_len lhs_end
    rtree = splitEvents' rhs (len - lhs_len) endTime

    runTime = runTimeOf ltree + runTimeOf rtree
    gcTime  = gcTimeOf  ltree + gcTimeOf  rtree


splitEventList :: [EventDuration]
               -> [EventDuration]
               -> Timestamp
               -> Timestamp
               -> Timestamp
               -> Int
               -> ([EventDuration], Int, Timestamp, [EventDuration])
splitEventList []     acc _tsplit tmax _tright len 
  = (reverse acc, len, tmax, [])
splitEventList (e:es) acc !tsplit !tmax !tright !len
  | tend == tright
  = (reverse acc, len, tmax, e:es)
      -- if the end of this event touches the right-hand boundary, then
      -- we cannot split any further.  Either we have a completely
      -- unsplittable sequence (acc == []), or we have managed to
      -- split some events off the beginning.  This case is quite important;
      -- without it we can end up putting all the events in the left branch
      -- and not making any progress at all.  This way we get to put a
      -- complete sub-sequence on the right.
  | tstart < tsplit -- pick all events that start before the split
  = splitEventList es (e:acc) (max tsplit tend)
                              (max tmax   tend) tright (len+1)
      -- although the input events are sorted by start time, they may
      -- be nested: e.g. a ThreadRun may have instantaneous
      -- CreateThread events within its duration.  We need to keep track
      -- of the end time of this event group (tmax), and also adjust the
      -- split point to make sure we catch all the instantaneous events
      -- that occur within the duration of events we have already seen.
  | otherwise
  = (reverse acc, len, tmax, e:es)
  where
    tstart = timeOfEventDuration e
    tend   = endTimeOfEventDuration e

-------------------------------------------------------------------------------

runTimeOf :: EventTree -> Timestamp
runTimeOf (EventSplit _ _ _ _ _ _ runTime _) = runTime
runTimeOf (EventTreeLeaf eventList)
  = sum [e - s | ThreadRun _ _ s e <- eventList]

-------------------------------------------------------------------------------

eventTreeEvents :: EventTree -> Int
eventTreeEvents (EventSplit _ _ _ _ _ len _ _) = len
eventTreeEvents (EventTreeLeaf eventList) = length eventList

-------------------------------------------------------------------------------

gcTimeOf :: EventTree -> Timestamp
gcTimeOf (EventSplit _ _ _ _ _ _ _ gcTime) = gcTime
gcTimeOf (EventTreeLeaf eventList)
  = sum [e - s | GC s e <- eventList]

-------------------------------------------------------------------------------

lastEventTime :: EventTree -> Timestamp
lastEventTime (EventSplit _ _ endTime _ _ _ _ _) = endTime
lastEventTime (EventTreeLeaf eventList)
  = endTimeOfEventDuration (last eventList)

-------------------------------------------------------------------------------

-- The HEC data structure is a list of pairs where each pair records
-- the unqiue ID of a HEC and its event information represented
-- using the EventTree data-structure.
type HECs = [(Int, EventTree)]

type MaybeHECsIORef = IORef  (Maybe HECs)

-------------------------------------------------------------------------------

ennumerateCapabilities :: [GHCEvents.Event] -> [Int]
ennumerateCapabilities events = 
  -- trace ("ennumerateCapabilities: " ++ show n_caps) $ 
  [0.. n_caps-1]
  where n_caps = head [ caps | GHCEvents.Event _ _ (GHCEvents.Startup caps) <- events ]

-------------------------------------------------------------------------------

-- Find the last timestamp value 
findLastTxValue :: HECs -> Timestamp
findLastTxValue hecs
  = maximum (map (lastEventTime . snd) hecs)

-------------------------------------------------------------------------------


-- Origin for graph

ox :: Int
ox = 10

oy :: Int
oy = 30

-- Origin for capability bars

oycap :: Int
oycap = 60

-- Gap betweem capabilities

gapcap :: Int
gapcap = 55

-- Bar height

barHeight :: Int
barHeight = 20

-------------------------------------------------------------------------------

-- This scale value is used to map a micro-second value to a pixel unit.
-- To convert a timestamp value to a pixel value, multiply it by scale.
-- To convert a pixel value to a micro-second value, divide it by scale.
-- A negative value means the scale value to be computed to fit the
-- trace to the display.

defaultScaleValue :: Double
defaultScaleValue = -1.0

-------------------------------------------------------------------------------
-- Scale a timetamp value by a value s giving an Int

tsScale :: Timestamp -> Double -> Int
tsScale t s = scaleIntegerBy (toInteger t) s

-------------------------------------------------------------------------------

scaleIntegerBy :: Integral int => int -> Double -> Int
scaleIntegerBy i d
  = truncate (fromIntegral i * d)

-------------------------------------------------------------------------------

