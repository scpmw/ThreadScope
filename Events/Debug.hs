
module Events.Debug (
  DebugMaps,
  Span(..), IPRange(..), DebugEntry(..), DebugEntryCore(..),

  emptyDebugMaps, buildDebugMaps,

  findDbgElem, findWithDbgElem, extSources,
  dumpDebug,

  lookupRange, lookupRanges,

  lookupCore, findAllocationEntry,

  getSumTimeTree, getSampleTimeTree, SampleWeight(..)

  ) where

import Prelude

import Events.Core as Core
import Events.TimeTree

import GHC.RTS.Events

import Data.Array
import Data.Char (chr, ord)
import Data.Maybe (mapMaybe, isJust, fromMaybe)
import Data.Monoid (Monoid(..))
import Data.List
import Data.Ord  (comparing)
import Data.Word (Word64)
import qualified Data.Function as F
import qualified Data.IntMap as IM
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.Array.Unboxed as UA

import Text.Printf

import Debug.Trace

type CapNo = Int

type RangeMap = IM.IntMap DebugEntry
type DbgMap = [DebugEntry]
type CoreMap = Map.Map (BS.ByteString, BS.ByteString, BS.ByteString) DebugEntry
type SourceMap = Map.Map Span [DebugEntry]

data Span = Span {
  fileName    :: BS.ByteString,
  spanName    :: BS.ByteString,
  startLine   :: {-# UNPACK #-} !Int,
  startCol    :: {-# UNPACK #-} !Int,
  endLine     :: {-# UNPACK #-} !Int,
  endCol      :: {-# UNPACK #-} !Int
  } deriving (Eq, Ord)

data IPRange = IPRange {
  rangeStart :: {-# UNPACK #-} !Int,
  rangeEnd   :: {-# UNPACK #-} !Int
  }
instance Show IPRange where
  show (IPRange x y) = printf "%06x-%06x" x y

data DebugEntry = DebugEntry {
  dbgId     :: {-# UNPACK #-} !Int, -- for quick identification checks
  dbgUnit   :: !BS.ByteString,
  dbgLabel  :: !BS.ByteString,
  dbgRanges :: [IPRange],
  dbgDName  :: Maybe BS.ByteString,
  dbgInstr  :: Maybe Int,
  dbgParent :: Maybe DebugEntry,
  dbgChilds :: [DebugEntry],
  dbgSources:: [Span],
  dbgDCore  :: Maybe DebugEntryCore
  }

data DebugEntryCore = DebugEntryCore {
  dbgCoreBind :: !BS.ByteString,
  dbgCoreCons :: !BS.ByteString,
  dbgCoreCode :: CoreExpr -- lazy
  }

instance Eq DebugEntry where
  (==)    = (==) `F.on` dbgId
instance Ord DebugEntry where
  compare = compare `F.on` dbgId

newtype SampleWeight = SampleWeight { unSampleWeight :: Word64 } deriving (Show)

instance Monoid SampleWeight where
  mempty                          = SampleWeight 0
  (SampleWeight a) `mappend` (SampleWeight b) = SampleWeight $ a + b

data SampleMap = SampleMap {
  smapCap :: CapNo,
  smapVerb :: SampleVerb,
  smapNoun :: SampleNoun,
  smapSamples :: IM.IntMap Event, -- ^ All matching samples, by time
  smapSampleWeight :: TimeTree SampleWeight   -- ^ Sum of weights over a given time period
  }

data DebugMaps = DebugMaps {
  rangeMap   :: RangeMap,
  coreMap    :: CoreMap,
  _sourceMap :: SourceMap,
  sampleMaps :: [SampleMap],
  endTime    :: Timestamp
  }


emptyDebugMaps :: DebugMaps
emptyDebugMaps = DebugMaps IM.empty Map.empty Map.empty [] 0

buildDebugMaps :: Array Int CapEvent -> (String -> IO ()) -> IO (DebugMaps, Array Int CapEvent)
buildDebugMaps eventsArr progress = do

  progress "Reading debug data..."

  -- Load debug data from event log
  let (dbgMap, startIx) = buildDbgMap eventsArr
  putStrLn $ printf "Debug data has %d procedures" (length dbgMap)
  -- mapM_ dumpDebug dbgMap

  -- Build range map
  let rangeMap = buildRangeMap dbgMap
  putStrLn  $ printf "Range map has %d entries" (IM.size rangeMap)
  --flip mapM_ (IM.assocs rangeMap) $ \(x, dbg) ->
  --  printf "%016x: %s\n" x (bsToStr (dbgLabel dbg))

  -- Build core map
  let coreMap = buildCoreMap dbgMap
  putStrLn $ printf "Core map has %d entries" (Map.size coreMap)
  -- mapM_ print $ Map.keys coreMap

  -- Build source map
  let sourceMap = buildSourceMap dbgMap
  --putStrLn $ printf "Source map has %d entries" (Map.size sourceMap)

  -- Build sample map
  let lastIx = snd $ bounds eventsArr
      endTime | lastIx > snd (bounds eventsArr)              = 0
              | CapEvent _ (Event t _) <- eventsArr ! lastIx = t
  let sampleMaps = buildSampleMaps eventsArr startIx endTime
      sampleCount = IM.size . smapSamples
  putStrLn $ printf "Sample map has %d types, %d entries total"
    (length sampleMaps) (sum $ map sampleCount sampleMaps)

  return (DebugMaps rangeMap coreMap sourceMap sampleMaps endTime, eventsArr)

-------------------------------------------------------------------------------

dumpDebug :: DebugEntry -> IO ()
dumpDebug DebugEntry{..} = do
  putStrLn $ bsToStr dbgUnit ++ "/" ++ bsToStr dbgLabel ++ ", " ++
    "IP ranges " ++ show dbgRanges ++ ", " ++
    (maybe "no dbg name" (("dbg name " ++).bsToStr) dbgDName) ++ ", " ++
    (maybe "no instr" (("instr " ++) . show) dbgInstr) ++ ", " ++
    (maybe "no parent" (\DebugEntry{..} -> "parent " ++ bsToStr dbgUnit ++ "/" ++ bsToStr dbgLabel) dbgParent) ++ ", " ++
    show (length dbgSources) ++ " source ranges, " ++
    (maybe "no core" (("core " ++) . show . dbgCoreCode) dbgDCore)

buildDbgMap :: Array Int CapEvent -> (DbgMap, Int)
buildDbgMap arr = result
  where
    (start, end) = bounds arr
    (maps, result) = go start (BS.empty) 0 0 maps (IM.empty, IM.empty) []

    -- The somewhat complicated knot tying here is a bit silly, given
    -- that at some point we want to have unique instrumentation IDs
    -- anyway. However, as currently the offset stuff doesn't seem to work,
    -- we work around that by building one IntMap per module.

    go n mfile moff !i ~(iMapI, cMapI) (iMapO, cMapO) xs
      | n > end   = ((iMapO, cMapO), (xs, n))
      | otherwise = case spec $ ce_event $ arr ! n of
        CreateThread {}
          -> ((iMapO, cMapO), (xs, n)) -- Don't expect any further debug data
        DebugModule { file }
          -> let res = go (n+1) file moff i (fst res) (IM.empty, IM.empty) xs
             in ((iMapO, cMapO), snd res)
        DebugProcedure { instr, parent, label }
          -> let (!n', !srcs, !ranges, !core) = go_proc (n+1) [] [] Nothing
                 parent_entry = do pid <- parent
                                   IM.lookup (fI pid) iMapI
                 child_entries = fromMaybe [] $ do id <- instr
                                                   IM.lookup (fI id) cMapI
                 name = case find ((== mfile) . fileName) srcs of
                   Just span              -> Just $ spanName span
                   Nothing | null srcs    -> Nothing
                           | otherwise    -> Just $ spanName $ head srcs
                 !entry = DebugEntry { dbgId = i
                                     , dbgUnit = mfile
                                     , dbgLabel = label
                                     , dbgRanges = ranges
                                     , dbgDName = name
                                     , dbgInstr = fmap (fI.(+moff).fI) instr
                                     , dbgParent = parent_entry
                                     , dbgChilds = child_entries
                                     , dbgSources = srcs
                                     , dbgDCore = core
                                     }
                 !iMapO' = case instr of
                   Just ix -> IM.insert (fI ix) entry iMapO
                   Nothing -> iMapO
                 !cMapO' = case parent of
                   Just pix -> IM.insertWith (++) (fI pix) [entry] cMapO
                   Nothing  -> cMapO
             in go n' mfile moff (i+1) (iMapI, cMapI) (iMapO', cMapO') (entry:xs)
        _other -> go (n+1) mfile moff i (iMapI, cMapI) (iMapO, cMapO) xs

    go_proc n srcs ranges core
      | n > end   = (n, reverse srcs, ranges, core)
      | otherwise = case spec $ ce_event $ arr ! n of
        DebugSource { sline, scol, eline, ecol, file, name=name' }
          -> let !span = Span file name' (fI sline) (fI scol) (fI eline) (fI ecol)
             in go_proc (n+1) (span:srcs) ranges core
        DebugPtrRange { low, high }
          -> go_proc (n+1) srcs (IPRange (fromIntegral low) (fromIntegral high):ranges) core
        DebugCore { coreBind, coreCons, coreCode }
          | not $ isJust core
          -> let core' = DebugEntryCore coreBind coreCons (getCoreExpr coreCode)
             in go_proc (n+1) srcs ranges (Just core')
        _other
          -> stop
      where stop = (n, reverse srcs, ranges, core)

    fI :: (Integral a, Integral b) => a -> b
    fI = fromIntegral


-- | Searches for an entry having the given property by traversing the
-- tree upwards. Returns the entry in question
findWithDbgElem :: (DebugEntry -> Maybe a) -> Maybe DebugEntry -> Maybe DebugEntry
findWithDbgElem f d@(Just dbg)    | Just _ <- f dbg = d
findWithDbgElem f (Just (DebugEntry { dbgParent })) = findWithDbgElem f dbgParent
findWithDbgElem _ Nothing                           = Nothing

-- | As findWithDbgElem, but returns the value
findDbgElem :: (DebugEntry -> Maybe a) -> Maybe DebugEntry -> Maybe a
findDbgElem f = (>>= f) . findWithDbgElem f

-- | Returns source code annotations to show for the given debug entry
extSources :: DebugEntry -> [Span]
extSources DebugEntry { dbgSources, dbgDName = Nothing, dbgParent = Just p}
  = dbgSources ++ extSources p
extSources DebugEntry { dbgSources}
  = dbgSources

-------------------------------------------------------------------------------

buildRangeMap :: [DebugEntry] -> RangeMap
buildRangeMap dbgs =
  let -- Convert input into a tuple list with one entry per range. Sort.
      ranges = [ (fromIntegral l, fromIntegral h, dbg)
               | dbg <- dbgs, IPRange l h <- dbgRanges dbg ]
      low  (l,_,_) = l
      high (_,h,_) = h
      dbg  (_,_,d) = d
      sorted = sortBy (compare `F.on` low) ranges

      -- Scans range list and inserts all ranges into a map.
      down :: [(Int, Int, DebugEntry)] -> [(Int, Int, DebugEntry)]
              -> RangeMap -> RangeMap
      down []     []      !rm = rm
      down (s:ss) []      !rm = up ss []     (high s) rm
      down []     (r:rs)  !rm = down [r] rs           (IM.insert (low r) (dbg r) rm)
      down (s:ss) (r:rs)  !rm
        | high s <= low r     = up ss (r:rs) (high s) rm
        | otherwise           = down (r:s:ss) rs      (IM.insert (low r) (dbg r) rm)

      -- Called to remove items from the stack, maybe re-inserting
      -- some ranges that were overriden but still apply. Will revert
      -- to "down" behaviour once no more ranges can be popped from
      -- the stack.
      up :: [(Int, Int, DebugEntry)] -> [(Int, Int, DebugEntry)] -> Int
            -> RangeMap -> RangeMap
      up []     rs _   !rm = down [] rs rm
      up (s:ss) rs p   !rm
        | high s > p       = down (s:ss) rs (IM.insert p (dbg s) rm)
        | otherwise        = up   ss rs p rm

  in down [] sorted IM.empty

-- | Lookup an instruction pointer in the range map.
lookupRange :: DebugMaps -> Int -> Maybe DebugEntry
lookupRange maps ip
  = -- use lookupLE here once container-0.5 is common enough...
    case IM.splitLookup ip $ rangeMap maps of
    (_, Just r, _)           -> Just r
    (lowerRanges, _, _)
      | IM.null lowerRanges  -> trace msg $ Nothing
      | (_, r) <- IM.findMax lowerRanges
        , any (ip `inIPRange`) (dbgRanges r)
                             -> Just r
      | otherwise            -> trace msg $ Nothing

  where msg = printf "Could not place IP %08x" ip

-- | Looks up a number of ranges, groups together same ranges
lookupRanges :: DebugMaps -> [(Int, Word64)] -> [(Int, Maybe DebugEntry)]
lookupRanges maps ips =
  let -- Check whether the first IP has a match in the range map
      go []      _  res = res
      go is0@((_,ip):is) rm res = case IM.splitLookup ip rm of
        (_,     Just d,  rm')         -> found 0 is0 d rm' (get_bound rm' d) res
        (lower, Nothing, rm')
          | IM.null lower             -> not_found ip is rm' res
          | (_,d) <- IM.findMax lower -> found 0 is0 d rm' (get_bound rm' d) res
          | otherwise                 -> not_found ip is rm' res

      -- Found a (possibly) matching debug entry, count number of IPs
      -- that actually match, then continue with reduced IPs and range
      -- map as well as another entry in the hits list..
      found n ((w,ip):is) d rm bound res
        | ip < bound && any (ip `inIPRange`) (dbgRanges d)
          = found (n+w) is d rm bound res
      found n ips d rm _ (hs,ms)
          = go ips rm $! (if n > 0 then (n,Just d):hs else hs, ms)

      -- Find where we need to stop matching IPs - either because the
      -- next (sub-)block starts or because our IP range ends.
      get_bound rm d
        | IM.null rm
        = maximum $ map rangeEnd $ dbgRanges d
        | (next,_) <- IM.findMin rm
        = let ranges' = filter ((< next) . rangeStart) $ dbgRanges d
          in maximum $ map (min next . rangeEnd) ranges'

      not_found ip is rm (hs, ms)
        = trace (printf "Could not place IP %08x" ip) $
          go is rm (hs, ms+1)

      process (w, p) = (w, fromIntegral p)
      preprocessed   = sortBy (comparing snd) $ map process ips
      (hits, misses) = go preprocessed (rangeMap maps) ([], 0)
   in if misses > 0
      then (misses, Nothing):hits
      else hits

inIPRange :: Int -> IPRange -> Bool
inIPRange ip (IPRange l h) = l <= ip && ip < h

-------------------------------------------------------------------------------

buildCoreMap :: [DebugEntry] -> CoreMap
buildCoreMap = Map.fromList . mapMaybe coreData
  where coreData entry@DebugEntry {dbgUnit, dbgDCore=Just core}
          = Just ((dbgUnit, dbgCoreBind core, dbgCoreCons core), entry)
        coreData _
          = Nothing

lookupCore :: DebugMaps -> String -> String -> String -> Maybe DebugEntry
lookupCore DebugMaps{coreMap=coreMap} unit bind cons
  = Map.lookup (strToBS unit, strToBS bind, strToBS cons) coreMap

findAllocationEntry :: DebugMaps -> DebugEntry -> DebugEntry
findAllocationEntry maps dbg =
  case fmap (findAllocation . dbgCoreCode) (dbgDCore dbg) of
    Just (Binds bs)
      | [child] <- filter (not . checkNoAlloc maps) (mapMaybe findBind bs)
           -> findAllocationEntry maps child
    _other -> dbg
 where findBind (b, c) = Map.lookup (dbgUnit dbg, b, c) (coreMap maps)

checkNoAlloc :: DebugMaps -> DebugEntry -> Bool
checkNoAlloc maps dbg =
  case fmap (findAllocation . dbgCoreCode) (dbgDCore dbg) of
    Just (Binds bs)
      | all (checkNoAlloc maps) $ mapMaybe findBind bs
                 -> True
    Just NoAlloc -> True
    _other       -> False
 where findBind (b, c) = Map.lookup (dbgUnit dbg, b, c) (coreMap maps) 

-------------------------------------------------------------------------------

buildSourceMap :: [DebugEntry] -> SourceMap
buildSourceMap = Map.fromListWith (++) . concatMap mkEntry
  where mkEntry entry@DebugEntry{dbgSources} =
          map (\s -> (s, [entry])) dbgSources

-------------------------------------------------------------------------------

lookupSampleMap :: [SampleMap] -> CapNo -> SampleVerb -> SampleNoun -> Maybe SampleMap
lookupSampleMap []                     _    _     _
  = Nothing
lookupSampleMap (m@SampleMap{..}:maps) cap verb noun
  | cap == smapCap && verb == smapVerb && noun == smapNoun
  = Just m
  | otherwise
  = lookupSampleMap maps cap verb noun

buildSampleMaps :: Array Int CapEvent -> Int -> Timestamp -> [SampleMap]
buildSampleMaps arr n endTime = map setSampleWeights $ go n []
  where
    (_, end) = bounds arr
    go n maps
      | n > end   = maps
      | CapEvent _ ev@(Event t (Samples {cap, sample_by, sample_type})) <- arr ! n
                  = let update []
                          = [SampleMap { smapCap = cap
                                       , smapVerb = sample_by
                                       , smapNoun = sample_type
                                       , smapSamples = IM.singleton (fromIntegral t) ev 
                                       , smapSampleWeight = undefined }]
                        update (om@SampleMap{..}:maps)
                          | cap == smapCap && sample_by == smapVerb && sample_type == smapNoun
                          = let !samples' = IM.insert (fromIntegral t) ev smapSamples
                            in om{smapSamples = samples'} : maps
                          | otherwise
                          = let !maps' = update maps in om:maps'
                    in go (n+1) $! update maps
      | otherwise = go (n+1) maps
    setSampleWeights m =
      let !weightTree = mkTimeTree (map weight $ IM.elems $ smapSamples m) endTime
          weight (Event t (Samples {weights})) = (t, SampleWeight $ sum $ UA.elems weights)
          weight _                             = error "weight: impossible"
      in m { smapSampleWeight = weightTree  }

-- | Gives a time tree for how much total sample weight we have for
-- every given time period
getSumTimeTree :: DebugMaps -> Int -> SampleVerb -> SampleNoun -> TimeTree SampleWeight
getSumTimeTree DebugMaps{sampleMaps} cap verb noun
  | Just smap <- lookupSampleMap sampleMaps cap verb noun
  = smapSampleWeight smap
  | otherwise
  = mkTimeTree [] 0

-- | Returns a time tree with the weights of all samples that fall
-- into any of the ranges given
getSampleTimeTree :: DebugMaps -> Int -> SampleVerb -> SampleNoun -> [DebugEntry] -> TimeTree SampleWeight
getSampleTimeTree dmaps@DebugMaps{sampleMaps,endTime} cap verb noun entries
  | Just smap <- lookupSampleMap sampleMaps cap verb noun
  = let ranges = getExclusiveIPRanges dmaps entries
        -- Bounds of ranges - small optimization. If this function
        -- ever becomes a bottleneck, one optimization might be to
        -- build a more fine-grained estimation for what memory
        -- regions we are interested in.
        !left = fromIntegral $ minimum $ 0 : map rangeStart ranges
        !right = fromIntegral $ maximum $ 0 : map rangeEnd ranges
        build (Event t (Samples {samples=samples, weights=weights})) evs
          = let (low, high) = UA.bounds samples
                go !i !acc
                  | i > high   = acc
                  | ip < left  = go (i+1) acc
                  | ip > right = go (i+1) acc
                  | any (fromIntegral ip `inIPRange`) ranges
                               = go (i+1) (acc + weights UA.! i)
                  | otherwise  = go (i+1) acc
                  where ip = samples UA.! i
            in case go low 0 of
              0 | (_, SampleWeight 0):_ <- evs
                -> (t, SampleWeight 0):tail evs
              w -> (t, SampleWeight w):evs
        build _other _ = error "getSampleTimeTree: impossible"
                         -- buildSampleMaps should have made sure of that
    in mkTimeTree (IM.foldr build [] (smapSamples smap)) endTime
  | otherwise
  = mkTimeTree [] 0

-- | Gives IP ranges for given debug entries. Also sorts & optimizes the ranges.
getExclusiveIPRanges :: DebugMaps -> [DebugEntry] -> [IPRange]
getExclusiveIPRanges DebugMaps{rangeMap} entries =
  let rawRanges = sortBy (comparing rangeStart) $ concatMap dbgRanges entries

      -- Some IP ranges might overlap -- this is mainly an optimization.
      merge (r1@(IPRange s1 e1) : rs1@((IPRange s2 e2) : rs2))
        | s2 <= e1   = merge (IPRange s1 (e1 `max` e2) : rs2)
        | otherwise  = r1 : merge rs1
      merge rs       = rs
      mergedRanges = merge rawRanges

      -- IP ranges might actually theoretically belong to other, more
      -- specific, DebugEntries. We check the rangeMap to make sure of
      -- that.
      extractRanges (IPRange start end) = go Nothing $ IM.assocs rmap
        where rmap = fst $ IM.split (end+1) $
                     snd $ IM.split (start-1) rangeMap
              go Nothing  []         = []
              go (Just x) []         = [IPRange x end]
              go Nothing  ((t,e):rs)
                | e `elem` entries   = go (Just t) rs
                | otherwise          = go Nothing  rs
              go (Just x) ((t,e):rs)
                | e `elem` entries   = go (Just x) rs
                | otherwise          = (IPRange x t) : go Nothing rs
              -- NB: The loop gets started with "Nothing", implying
              -- that not encountering a range map entry exactly at
              -- "start" means that the start of the range isn't
              -- covered. This is, in fact, true because the range map
              -- was generated from debug entries, therefore the
              -- unlikely case of our "start" not appearing means that
              -- we actually were overwritten by a range that's not a
              -- member of "entries".

  in concatMap extractRanges mergedRanges

-------------------------------------------------------------------------------
-- Utils

bsToStr :: BS.ByteString -> String
bsToStr = map (chr.fromIntegral) . BS.unpack

strToBS :: String -> BS.ByteString
strToBS = BS.pack . map (fromIntegral.ord)

