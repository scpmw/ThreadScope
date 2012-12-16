
module Events.TimeTree
  ( TimeTree
  , timeTreeNull
  , timeTreeLeaf
  , timeTreeStart
  , timeTreeEnd
  , timeTreeDelta
  , timeTreeVal
  , timeTreeLeft
  , timeTreeRight
  , timeTreeMaxDepth
  , timeTreeSize
  , mkTimeTree
  , mkTimeSubTree
  , sliceTimeTree
  , traceTimeTree
  ) where

import GHC.RTS.Events (Timestamp)

import Control.Monad
import Data.Monoid

import System.IO.Unsafe -- only for debugging, I swear

-- | Maps node data onto a binary search tree, so that we can easily
-- find the nodes that corresponds to a particular region of the
-- timeline.  Additionally, each node of the tree contains a summary
-- of the information below it, so that we can render views at various
-- levels of resolution. For example, if a tree node would represent
-- less than one pixel on the display, there is no point is descending
-- the tree further.
data TimeTree a
  = TimeTree
      {-#UNPACK#-}!Timestamp  -- ^ start time of span represented by the tree
      {-#UNPACK#-}!Timestamp  -- ^ end time of the span represented by the tree
      (TimeNode a)
  deriving Show

data TimeNode a
  = TimeSplit
      {-#UNPACK#-}!Timestamp  -- ^ time used to split the span into two parts
      (TimeNode a)
        -- ^ the LHS split; all data lies completely between start and split
      (TimeNode a)
        -- ^ the RHS split; all data lies completely between split and end
      a
        -- ^ aggregate of the spark stats within the span
  | TimeTreeLeaf
      a
        -- ^ the spark stats for the base duration
  | TimeTreeEmpty
      -- ^ represents a span that no data referts to, e.g., after the last GC
  deriving Show

-- | Returns whether the time tree is empty
timeTreeNull :: TimeTree a -> Bool
timeTreeNull (TimeTree _ _ TimeTreeEmpty) = True
timeTreeNull _other                       = False

-- | Returns whether the time tree only contains one node
timeTreeLeaf :: TimeTree a -> Bool
timeTreeLeaf (TimeTree _ _ TimeTreeLeaf{}) = True
timeTreeLeaf _other                        = False

-- | Gets the start time represented by the time tree
timeTreeStart :: TimeTree a -> Timestamp
timeTreeStart (TimeTree s _ _) = s

-- | Gets the end time represented by the time tree
timeTreeEnd :: TimeTree a -> Timestamp
timeTreeEnd (TimeTree _ e _) = e

-- | Gets the time delta spanned by a time tree
timeTreeDelta :: TimeTree a -> Timestamp
timeTreeDelta (TimeTree s e _) = e - s

-- | Gets the (possibly aggregated) value of a non-empty time tree
timeTreeVal :: Monoid a => TimeTree a -> a
timeTreeVal (TimeTree _ _ n) = case n of
  TimeTreeLeaf    v -> v
  TimeSplit _ _ _ v -> v
  _other            -> mempty

-- | Gets the "left" subtree of a time tree, if possible
timeTreeLeft :: TimeTree a -> TimeTree a
timeTreeLeft (TimeTree s _ n) = case n of
  TimeSplit t l _ _ -> TimeTree s t l
  _other            -> error "timeTreeVal: Non-split tree!"

-- | Gets the "right" subtree of a time tree, if possible
timeTreeRight :: TimeTree a -> TimeTree a
timeTreeRight (TimeTree _ e n) = case n of
  TimeSplit t _ r _ -> TimeTree t e r
  _other            -> error "timeTreeVal: Non-split tree!"

-- | Returns the depth of the tree
timeTreeMaxDepth :: TimeTree a -> Int
timeTreeMaxDepth (TimeTree _ _ t) = nodeDepth t
  where nodeDepth (TimeSplit _ lhs rhs _) = 1 + nodeDepth lhs `max` nodeDepth rhs
        nodeDepth _other                  = 1

-- | Returns the number of nodes in the tree
timeTreeSize :: TimeTree a -> Int
timeTreeSize (TimeTree _ _ t) = nodeSize t
  where nodeSize (TimeSplit _ lhs rhs _) = 1 + nodeSize lhs + nodeSize rhs
        nodeSize _other                  = 1

-- | Create time tree from spark durations.
-- Note that the last event may be not a spark event, in which case
-- there is no data about sparks for the last time interval
-- (the subtree for the interval will have SparkTreeEmpty node).
mkTimeTree :: Monoid a
           => [(Timestamp, a)] -- ^ node data
           -> Timestamp        -- ^ end time of last event in the list
           -> TimeTree a
mkTimeTree es endTime =
  let !(tree, _) = split es endTime
      (s, e) | ((t,_):_) <- es = (t, endTime)
             | otherwise       = (0, 0)
   in TimeTree s e tree

-- | Construct spark tree, by recursively splitting time intervals.
-- We only split at existing node times. Therefore, the binary tree is
-- only roughly split by time, the actual split depends on the
-- distribution of sample points below it.
split ::  Monoid a => [(Timestamp, a)] -> Timestamp -> (TimeNode a, [(Timestamp, a)])
split []     !_endTime
  = (TimeTreeEmpty, [])
split (e:es) !endTime
  | null es || fst (head es) + 1 >= endTime
  = (TimeTreeLeaf $! snd e, es)
  -- NB: The "+1" means that we will start making leaf nodes when the
  --     interval gets too small to split meaningfully. This is okay
  --     with the recursion below, as it is not actually relying on
  --     endTime getting observed in any way. As long as we make
  --     progress, we're fine.
split es     !endTime
  = let -- Choose a time to split at, construct tree up to that point
        startTime     = fst $ head es
        !splitTime    = startTime + (endTime - startTime) `div` 2
        !(ltree, rhs) = split es splitTime
       -- Any nodes left on right-hand side?
    in case rhs of
      (realSplit,_):_
        | realSplit < endTime
        -> -- Construct tree for right side
           let !(rtree, rest) = split rhs endTime
               val            = let !lval = nodeVal ltree
                                    !rval = nodeVal rtree
                                in lval `mappend` rval
           in (TimeSplit realSplit ltree rtree val, rest)
      _ -> (ltree, rhs)

nodeVal :: Monoid a => TimeNode a -> a
nodeVal TimeTreeEmpty       = mempty
nodeVal (TimeTreeLeaf a)    = a
nodeVal (TimeSplit _ _ _ a) = a

-- | Gives the subtree containing just the nodes between the two given timestamps
mkTimeSubTree
  :: Monoid a
  => ((Timestamp, Timestamp) -> (Timestamp, Timestamp) -> a -> a)
                             -- ^ Leaf node clamping function.
                             --   Parameters are the old then and the new extend.
  -> (Timestamp, Timestamp)  -- ^ Bounds
  -> TimeTree a              -- ^ Input time tree
  -> TimeTree a
mkTimeSubTree clamp (lbound, rbound) tree@(TimeTree start end node)
  | lbound <= start && rbound >= end
    = tree
  | lbound >= end || rbound <= start
    = TimeTree lbound rbound TimeTreeEmpty
  | otherwise
    = case node of
      TimeTreeEmpty                          -> mkTree node
      TimeTreeLeaf v
        | lbound <= start && rbound >= end   -> mkTree node
        | otherwise                          ->
            let !left = max lbound start
                !right = min rbound end
                clamped = clamp (start, end) (left, right) v
            in mkTree $ TimeTreeLeaf clamped
      TimeSplit split _ _ _
        | lbound <= split && rbound <= split -> leftSubTree
        | lbound >= split && rbound >= split -> rightSubTree
        | otherwise                          -> merge split leftSubTree rightSubTree
 where leftSubTree = mkTimeSubTree clamp (lbound,rbound) $ timeTreeLeft tree
       rightSubTree = mkTimeSubTree clamp (lbound,rbound) $ timeTreeRight tree
       mkTree = TimeTree (lbound `max` start) (rbound `min` end)
       merge split t1@(TimeTree _ _ n1) t2@(TimeTree _ _ n2)
         | timeTreeNull t2 && timeTreeNull t2
                     = mkTree $ TimeTreeEmpty
         | otherwise = mkTree $ TimeSplit split n1 n2
                                   (timeTreeVal t1 `mappend` timeTreeVal t2)

-- | Slices a part of the tree into a number of equally sized subtrees
sliceTimeTree
  :: Monoid a
  => ((Timestamp, Timestamp) -> (Timestamp, Timestamp) -> a -> a)
                             -- ^ Leaf node clamping function.
                             --   Parameters are the old then and the new extend.
  -> (Timestamp, Timestamp)  -- ^ Bounds
  -> Timestamp               -- ^ Slice size
  -> TimeTree a
  -> [TimeTree a]
-- TODO: This could probably be optimized somehow?
sliceTimeTree clamp (lbound, rbound) slice tree
  = go lbound $ mkTimeSubTree clamp (lbound, rbound) tree
 where
   go left tree
     | left >= rbound = []
     | left >= timeTreeEnd tree
                      = mkTimeTree [] (left+slice) : go (left+slice) tree
     | otherwise      = let leftTree = mkTimeSubTree clamp (left, left+slice) tree
                            rightTree = mkTimeSubTree clamp (left+slice, rbound) tree
                        in leftTree : go (left+slice) rightTree


traceTimeTree :: Show a => String -> TimeTree a -> TimeTree a
traceTimeTree msg tree = unsafePerformIO $ putStrLn msg >> go 0 tree >> return tree
  where go ind tree@(TimeTree left right node) = do
          replicateM ind (putStr "  ")
          putStr $ concat [ show left, "-", show right, " (+" , show (right-left) ++ "): "]
          case node of
            TimeTreeEmpty     -> putStrLn "---"
            TimeTreeLeaf v    -> print v
            TimeSplit _ _ _ v -> do
              print v
              go (ind + 1) (timeTreeLeft tree)
              go (ind + 1) (timeTreeRight tree)
