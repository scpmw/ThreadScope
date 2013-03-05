
{-# LANGUAGE BangPatterns, RecordWildCards #-}

module Events.Tasks (
  TaskNode(..), TaskGraph, extractTaskGraph,
  TaskLayout, layoutTaskGraph, taskLayoutHeight
  ) where

import Control.DeepSeq

import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import Data.Ord (comparing)
import Data.List (maximumBy, find, nub)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Array

import GHC.RTS.Events

import Debug.Trace

type Id = Int

-- | A task node represents a time that the task was active.
data TaskNode =
  TaskNode { taskTid :: {-# UNPACK #-} !Int
           , taskCap :: {-# UNPACK #-} !Int
           , taskStart :: {-# UNPACK #-} !Timestamp
           , taskEnd :: {-# UNPACK #-} !Timestamp
           , taskParents :: [Id]
           , taskChilds :: [Id]
           }

instance Show TaskNode where
  show TaskNode{..} =
    "Task " ++ show taskTid ++ " Cap " ++ show taskCap ++ ": " ++
    show taskStart ++ "-" ++ show taskEnd ++ " " ++
    show taskParents ++ " " ++ show taskChilds

nodeLength :: TaskNode -> Timestamp
nodeLength n = taskEnd n - taskStart n + 1

-- External types
type TaskGraph = IM.IntMap TaskNode
type TaskLayout = IM.IntMap Int

type TaskId = Int
type NodeId = Int

data ExState =
  ExState { exNextNode :: !NodeId
            -- | currently active task on cap, with start time
          , exActive :: !(IM.IntMap (TaskId, Timestamp))
            -- | collected dependencies for next node of task
          , exDepends :: !(IM.IntMap [NodeId])
            -- | Last finished node for given task
          , exLast :: !(IM.IntMap NodeId)
            -- | The current graph
          , exGraph :: !TaskGraph
            -- | Children to insert
          , exChilds :: !(IM.IntMap [NodeId])
          }

extractTaskGraph :: Array Int CapEvent -> TaskGraph
extractTaskGraph = trace (show $ layoutTaskGraph testTaskGraph) $ cleanTaskGraph 100 . makeTaskGraph

-- | Reads the raw task graph from event log array.
makeTaskGraph :: Array Int CapEvent -> TaskGraph
-- TODO: We make a critical assumption here -- that a task which a
-- dependency is declared on is not currently running.
makeTaskGraph eventsArr =
 let makeNode cap taskId start end isSplit ExState{..} =
       let nodeId = exNextNode
           !parents = fromMaybe [] $ IM.lookup taskId exDepends
           !nd = TaskNode
                  { taskTid = taskId
                  , taskCap = cap
                  , taskStart = start
                  , taskEnd = end
                  , taskParents = parents
                  , taskChilds = {- lazy! -} fromMaybe [] $ IM.lookup nodeId finalChilds
                  }
           addAsChild pid = IM.insertWith (++) pid [nodeId]
       in ExState { exNextNode = nodeId + 1
                  , exActive = if isSplit
                               then IM.insert cap (taskId, end) exActive
                               else IM.delete cap exActive
                    -- clear depends, make next task node depend on previous one
                  , exDepends = IM.insert taskId [nodeId] exDepends
                  , exLast = IM.insert taskId nodeId exLast
                  , exGraph = IM.insert nodeId nd exGraph
                  , exChilds = foldr addAsChild exChilds parents
                  }
     (!startIx, !endIx) = bounds eventsArr
     go i state
       | i > endIx  = (exGraph state, exChilds state)
       | otherwise  =
           let CapEvent mcap (Event t spec) = eventsArr ! i
               findTask taskId = find ((== taskId) . fst . snd) (IM.assocs (exActive state))
           in case spec of
             StartTask _
               | Just cap <- mcap, Just (other, start) <- IM.lookup cap (exActive state)
               -> go i $ makeNode cap other start t False state
             StopTask _
               | Just cap <- mcap, Just (other, start) <- IM.lookup cap (exActive state)
               -> go (i+1) $ makeNode cap other start t False state
             StartTask taskId | Just cap <- mcap ->
               go (i+1) state { exActive = IM.insert cap (fromIntegral taskId, t) (exActive state) }
             TaskDependency taskOfW taskOnW ->
               let taskOf = fromIntegral taskOfW; taskOn = fromIntegral taskOnW
                   -- 1. Split parent node, if currently active
                   !state1 = case findTask taskOn of
                     Just (cap', (_, start)) -> makeNode cap' taskOn start t True state
                     Nothing                 -> state
                   -- 2. Register dependency, if there even *is* a
                   -- parent node we can locate (otherwise ignore)
                   !state2 = case IM.lookup taskOn (exLast state1) of
                     Just nodeId -> state1 {
                       exDepends = IM.insertWith (++) taskOf [nodeId] (exDepends state1) }
                     Nothing     -> state1
                   -- 3. Split child node, if currently active. If
                   -- not, the dependency will be added once the child
                   -- task becomes active.
                   !state3 = case findTask taskOf of
                     Just (cap', (_, start)) -> makeNode cap' taskOf start t True state2
                     Nothing                 -> state2
                   -- Note that most of this splitting will be undone
                   -- in the cleaning step, but there are graphs where
                   -- it is unavoidable and we can't tell until we
                   -- have a full overview of the graph.
               in go (i+1) state3
             _other -> go (i+1) state
     (result, finalChilds) = go startIx $ ExState 0 IM.empty IM.empty IM.empty IM.empty IM.empty
  in trace (show result) $ result

-- | Attempts to eliminate nodes that are smaller than the given
-- threshold.
--
-- This is allowed if:
-- 1. The nodes in question come from the same task
-- 2. The eliminated node has no extra outgoing (incoming) edges if prepended (appended)
-- 3. A merged node would not violate graph consistency
cleanTaskGraph :: Timestamp -> TaskGraph -> TaskGraph
cleanTaskGraph thresh graph0 =
  let (graph1, _) = IM.foldrWithKey' prepend (IM.empty, IM.empty) graph0
      (graph2, _) = IM.foldlWithKey' append (IM.empty, IM.empty) graph1
      -- step one: prepending
      prepend i nd (graph, ndMap)
        | -- task short enough
          taskLen     <- taskEnd nd - taskStart nd,
            taskLen < thresh,
          -- exactly one outgoing edge, same task
          [nextId0]   <- taskChilds nd,
          Just nextId <- IM.lookup nextId0 ndMap,
          Just next   <- IM.lookup nextId graph,
            taskTid next == taskTid nd,
          -- new start doesn't violate consistency
          newStart    <- taskStart next - taskLen,
          newParents  <- nub (taskParents nd ++
                              filter (/=i) (taskParents next)),
            all ((<= newStart) . taskEnd) (map (graph0 IM.!) newParents)
          -- hm, something about cap? we will merge nodes from
          -- different caps here...
          = let !newNext = next { taskParents = newParents
                                , taskStart = newStart
                                }
                !newGraph = IM.insert nextId newNext graph
                !newNdMap = IM.insert i nextId ndMap
            in (newGraph, newNdMap)
        | otherwise
          = let !newNd = nd { taskChilds = map (ndMap IM.!) (taskChilds nd) }
                !newGraph = IM.insert i newNd graph
                !newNdMap = IM.insert i i ndMap
            in (newGraph, newNdMap)
      -- step two: appending (exactly mirrored)
      append (graph, ndMap) i nd
        | -- task short enough
          taskLen     <- taskEnd nd - taskStart nd,
            taskLen < thresh,
          -- exactly one incoming edge, same task
          [prevId0]   <- taskParents nd,
          Just prevId <- IM.lookup prevId0 ndMap,
          Just prev   <- IM.lookup prevId graph,
            taskTid prev == taskTid nd,
          -- new start doesn't violate consistency
          newEnd      <- taskEnd prev + taskLen,
          newChilds   <- nub (taskChilds nd ++
                              filter (/=i) (taskChilds prev)),
            all ((>= newEnd) . taskStart) (map (graph0 IM.!) newChilds)
          -- hm, something about cap? we will merge nodes from
          -- different caps here...
          = let !newNext = prev { taskChilds = newChilds
                                , taskEnd = newEnd
                                }
                !newGraph = IM.insert prevId newNext graph
                !newNdMap = IM.insert i prevId ndMap
            in (newGraph, newNdMap)
        | otherwise
          = let !newNd = nd { taskParents = map (ndMap IM.!) (taskParents nd) }
                !newGraph = IM.insert i newNd graph
                !newNdMap = IM.insert i i ndMap
            in (newGraph, newNdMap)
  in trace (show graph2) $ graph2

-- | Calculates a layout for the given task graph.
layoutTaskGraph :: TaskGraph -> (Int, TaskLayout)
{--
-- Here's the idea:
--
-- While the graph does not *have* to be structured, we want to make
-- use of hierarchical properties in the graph as much as
-- possible. Therefore our strategy is to pick good "left" and "right"
-- node combinations, then recursively layout all un-layouted nodes on
-- the "path" between them.
layoutTaskGraph dag =
  let descs = descendants dag
      ancs = ancestors dag

      combos rs ls restrict =
        [ (lengthSum path', (r, l, path'))
        | r <- rs
        , let rdescs = descs IM.! r
        , l <- IS.toList (lset `IS.intersection` rdescs)
        , let path = rdescs `IS.intersection` (ancs IM.! l)
        , let path' = case restrict of
                Just nodes -> nodes `IS.intersection` path
                Nothing    -> path]
        where lset = IS.fromList ls

      updateCombos done = mapMaybe up
        where up (_, (r, l, path)) =
                let path' = path `IS.difference` done
                in if IS.null path' then Nothing else
                     Just (lengthSum path', (r, l, path'))

      -- TODO: special-case having only one combination
      go [] !blocks !lay = --trace (" b="++show blocks) $
                           (maximum $ IM.elems blocks, lay)
      go cs !blocks !lay = --trace ("r="++show bestR++" l="++show bestL++" p="++show bestPath++
                           --       " b="++show blocks++" y="++show y++" cs="++show cs) $
                           go cs' blocks' lay'
        where
          (bestR, bestL, bestPath) = snd $ maximumBy (comparing fst) cs
          bestRn = dag IM.! bestR
          bestLn = dag IM.! bestL

          -- The block we have to reserve goes from root to leaf --
          -- inclusive or exclusive depending on whether they have
          -- been layouted yet.
          !blkStart | bestR `IM.member` lay = fromIntegral $ taskEnd bestRn
                    | otherwise             = fromIntegral $ taskStart bestRn
          !blkEnd | bestL `IM.member` lay = fromIntegral $ taskStart bestLn
                  | otherwise             = fromIntegral $ taskEnd bestLn

          -- Extended bounds: Add some space
          !extStart = blkStart - (blkEnd - blkStart) `div` 2
          !extEnd = blkEnd + (blkEnd - blkStart) `div` 2

          -- Helper to find the first unblocked y position for a given x
          blockAt :: Int -> Int
          blockAt x = case lookupLE x blocks of
            Just (_, y) -> y
            _other      -> error "Impossible: blockAt failed block lookup!"

          -- Vertical position of new block
          blockers = fst $ IM.split extEnd $ snd $ IM.split extStart blocks
          !y = maximum (blockAt extStart : IM.elems blockers)

          -- place root and leaf node if necessary
          insert2 = IM.insertWith (flip const)
          recLay  = insert2 bestR y $ insert2 bestL y lay

          -- combos for nodes in between
          recCs      = combos (taskChilds bestRn) (taskParents bestLn) (Just bestPath)
          (y', lay') = go recCs (IM.singleton minBound y) recLay
          cs'        = updateCombos bestPath cs

          -- New blockers: Take over all blockers from the left,
          -- insert new blockers, duplicate blocker affecting right
          (blocksLeft, blocksRest) = IM.split blkStart blocks
          (_, blocksRight) = IM.split blkEnd blocksRest
          yblock = y' `max` (y + 1)
          !blocks' = IM.unions [ blocksLeft
                               , IM.singleton blkStart yblock
                               , IM.singleton blkEnd (blockAt blkEnd)
                               , blocksRight ]

      lengthSum = IS.foldr (\i x -> x + nodeLength (dag IM.! i)) 0

      roots = IM.keys $ IM.filter ((==[]) . taskParents) dag
      leaves = IM.keys $ IM.filter ((== []) . taskChilds) dag
  in go (combos roots leaves Nothing) (IM.singleton minBound 0) IM.empty
--}
layoutTaskGraph dag =
  let descs = descendants dag
      ancs = ancestors dag

      rcombos rs ls restrict =
        [ (lengthSum path, r, taskChilds (dag IM.! r), ls, path)
        | r <- rs, let path = (descs IM.! r) `IS.intersection` restrict, not (IS.null path)]
      lcombos rs ls restrict =
        [ (lengthSum path, l, rs, taskParents (dag IM.! l), path)
        | l <- ls, let path = (ancs IM.! l) `IS.intersection` restrict, not (IS.null path)]

      updateCombos done = mapMaybe up
        where up (_, n, rs, ls, path) =
                let path' = path `IS.difference` done
                in if IS.null path' then Nothing else
                     Just (lengthSum path', n, rs, ls, path')

      -- Catch as many simple cases as possible top-level to avoid
      -- having to actually compare paths (or leave them in memory).
      goTop rs ls start end lay path ind
        | null rs               = (0, start, end, lay)
        | null ls               = (0, start, end, lay)
        | [r] <- rs             = goSingleR r
        | [l] <- ls             = goSingleL l
        | [(_,r,_,_,_)] <- rcs  = goSingleR r
        | [(_,l,_,_,_)] <- lcs  = goSingleL l
        | otherwise             = go (rcs++lcs) rs ls initBlocks 0 lay start end ind
        where rcs = rcombos rs ls path
              lcs = lcombos rs ls path
              initBlocks = IM.singleton minBound IS.empty
              goSingle rs ls n = goTop rs ls start end (IM.insert n 0 lay) path ind
              goSingleR r = goSingle (taskChilds $ dag IM.! r) ls r
              goSingleL l = goSingle rs (taskParents $ dag IM.! l) l

      -- TODO: special-case having only one combination
      go [] _  _  _       !hgt !lay !start !end ind
        = trace (ind ++ "= hgt: " ++ show hgt) $
          (hgt, start, end, lay `deepseq` lay)
      go cs rs ls !blocks !hgt !lay !start !end ind
        = trace (ind ++ "before: " ++ show blocks) $
          trace (ind ++ "range = " ++ show extStart ++ "-" ++ show extEnd ++ " y = " ++ show y) $
          trace (ind ++ "blockers: " ++ show blockers ++ " hgt: " ++ show ownHgt) $
          trace (ind ++ "after: " ++ show blocks') $
          trace (ind ++ "hgt':" ++ show hgt') $
          go cs' rs ls blocks' hgt' (lay' `deepseq` lay') start' end' ind
        where
          len (l,_,_,_,_) = l
          (_, best, newRs, newLs, bestPath) = maximumBy (comparing len) cs
          !bestNd = dag IM.! best

          -- get layout for recursed nodes
          (blkHgt, blkStart, blkEnd, blkLay)
              = {-trace ("combs=" ++ show cs) $
                trace ("b= " ++ show best ++ " " ++ show (IS.toList bestPath)) $ -}
                goTop newRs newLs
                      maxBound minBound
                      IM.empty bestPath (' ':ind)
          cs' = updateCombos bestPath cs
          start' | nodeDone  = start `min` blkStart
                 | otherwise = start `min` blkStart `min` taskStart bestNd
          end'   | nodeDone  = end `max` blkEnd
                 | otherwise = end `max` blkEnd `max` taskEnd bestNd

          -- Extended bounds: Add some space
          extStart = fromIntegral $ blkStart - (blkEnd - blkStart) `div` 8
          extEnd = fromIntegral $ blkEnd + (blkEnd - blkStart) `div` 8

          -- Split up blockers
          !(!blocksRest, !blocksRight) = splitLT (extEnd+1) blocks
          !(!blocksLeft, !blocksMid) = splitLT extStart blocksRest
          !(_, !startBlocks) = IM.findMax blocksLeft

          -- Check whether our node has been layouted already. This
          -- happens if it was part of a path of a node from the
          -- "other side", which however didn't pick up all our
          -- ancestors/descendants at the same time.
          !nodeDone = best `IM.member` lay
          ownHgt | nodeDone  = blkHgt
                 | otherwise = 1 `max` blkHgt

          -- Vertical position of new block - the first gap that's big
          -- enough. I'm sure there's a smarter way of doing this.
          blockers     = IS.unions (startBlocks : IM.elems blocksMid)
          checkSpace y = IS.null $ snd $ IS.split (y-1) $ fst $ IS.split (y+ownHgt) blockers
          !y = case filter (checkSpace . (+1)) (IS.toList blockers) of
            []    -> 0
            (y:_) -> y+1
          lay1 = lay `IM.union` (IM.map (+y) blkLay)

          -- New layout and height, retaining existing layout for our node.
          lay' = IM.insertWith (flip const) best y lay1
          hgt' = hgt `max` (y + ownHgt)

          -- New blockers: Take over all blockers from the left,
          -- insert new blockers, duplicate blocker affecting right
          newBlocks = IS.fromList [y..y+ownHgt-1]
          blocks' =
            IM.union blocksLeft $!
            IM.insertWith (flip const) extStart (newBlocks `IS.union` startBlocks) $!
            IM.union (IM.map (IS.union newBlocks) blocksMid) $!
            (IM.insertWith (flip const) (extEnd+1) (snd $ IM.findMax blocksRest)) $!
            blocksRight

      lengthSum = IS.foldr (\i x -> x + nodeLength (dag IM.! i)) 0

      roots = IM.keys $ IM.filter (null . taskParents) dag
      leaves = IM.keys $ IM.filter (null . taskChilds) dag

      !(hgt, _, _, lay) = goTop roots leaves maxBound minBound IM.empty (IM.keysSet dag) ""
  in (hgt, lay)


-- | Splits the map into two maps with keys less than and greater than
-- *or equal* the given key respectively.
splitLT :: IM.Key -> IM.IntMap a -> (IM.IntMap a, IM.IntMap a)
splitLT k m = case IM.splitLookup k m of
  (lm, Nothing, rm) -> (lm, rm)
  (lm, Just x, rm)  -> (lm, IM.insert k x rm)


descendants :: TaskGraph -> IM.IntMap IS.IntSet
descendants dag = descs
  where descs = IM.mapWithKey getDescs dag
        getDescs i nd = IS.singleton i `IS.union`
                        IS.unions (map (descs IM.!) (taskChilds nd))

ancestors :: TaskGraph -> IM.IntMap IS.IntSet
ancestors dag = ancs
  where ancs = IM.mapWithKey getAncs dag
        getAncs i nd = IS.singleton i `IS.union`
                       IS.unions (map (ancs IM.!) (taskParents nd))

taskLayoutHeight :: TaskLayout -> Int
taskLayoutHeight = (+1) . maximum . (0:) . IM.elems

testTaskGraph :: TaskGraph
testTaskGraph = IM.fromList
  [ (1, taskNode 1 2 [] [2,8])
  , (2, taskNode 2 3 [1] [3,7])
  , (3, taskNode 3 4 [2] [4])
  , (4, taskNode 4 5 [3] [5])
  , (5, taskNode 5 6 [4,7] [6])
  , (6, taskNode 6 7 [5] [])
  , (7, taskNode 4 5 [2,10] [5])
  , (8, taskNode 2 4 [1] [10])
  , (9, taskNode 1 2 [] [10])
  , (10, taskNode 3 4 [8,9] [7,11])
  , (11, taskNode 4 5 [10] [12, 13])
  , (12, taskNode 5 6 [11] [14])
  , (13, taskNode 5 6 [11] [14])
  , (14, taskNode 6 7 [12,13] [15])
  , (15, taskNode 7 8 [14] []) ]
  where taskNode = TaskNode 1 1

embarassingTaskGraph :: TaskGraph
embarassingTaskGraph = IM.fromList
  [ (1, taskNode 1 2 [] [2,3,4,5,6,7,8,9,10,11,12])
  , (2, taskNode 2 3 [1] [13])
  , (3, taskNode 3 4 [1] [13])
  , (4, taskNode 4 5 [1] [13])
  , (5, taskNode 5 6 [1] [13])
  , (6, taskNode 6 7 [1] [13])
  , (7, taskNode 7 8 [1] [13])
  , (8, taskNode 8 9 [1] [13])
  , (9, taskNode 9 10 [1] [13])
  , (10, taskNode 10 11 [1] [13])
  , (11, taskNode 11 12 [1] [13])
  , (12, taskNode 5 13 [1] [13])
  , (13, taskNode 13 14 [2,3,4,5,6,7,8,9,10,11,12] [])
  ]
  where taskNode = TaskNode 1 1
