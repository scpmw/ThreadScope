
module Events.Debug (
  DebugMaps,
  Span(..), IPRange(..), DebugEntry(..), DebugEntryCore(..),

  emptyDebugMaps, buildDebugMaps,

  findDbgElem, findWithDbgElem, extSources,
  dumpDebug,

  lookupRange, lookupRanges,

  lookupCore, findAllocationEntry

  ) where

import Prelude

import Events.Core as Core

import GHC.RTS.Events

import Data.Array
import Data.Char (chr, ord)
import Data.Maybe (mapMaybe, isJust)
import Data.List
import Data.Word (Word64)
import qualified Data.Function as F
import qualified Data.IntMap as IM
import qualified Data.Map as Map
import qualified Data.ByteString as BS

import Text.Printf

import Debug.Trace

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
  _rangeStart :: {-# UNPACK #-} !Int,
  _rangeEnd   :: {-# UNPACK #-} !Int
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


data DebugMaps = DebugMaps {
  rangeMap :: RangeMap,
  coreMap :: CoreMap,
  _sourceMap :: SourceMap
  }


emptyDebugMaps :: DebugMaps
emptyDebugMaps = DebugMaps IM.empty Map.empty Map.empty

buildDebugMaps :: Array Int CapEvent -> (String -> IO ()) -> IO (DebugMaps, Array Int CapEvent)
buildDebugMaps eventsArr progress = do

  progress "Reading debug data..."

  -- Load debug data from event log
  let dbgMap = buildDbgMap eventsArr
  putStrLn $ printf "Debug data has %d procedures" (length dbgMap)
  -- mapM_ dumpDebug dbgMap

  -- Build range map
  let rangeMap = buildRangeMap dbgMap
  putStrLn  $ printf "Range map has %d entries" (IM.size rangeMap)

  -- Build core map
  let coreMap = buildCoreMap dbgMap
  putStrLn $ printf "Core map has %d entries" (Map.size coreMap)
  -- mapM_ print $ Map.keys coreMap

  -- Build source map
  let sourceMap = buildSourceMap dbgMap
  putStrLn $ printf "Source map has %d entries" (Map.size sourceMap)

  return (DebugMaps rangeMap coreMap sourceMap, eventsArr)

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

buildDbgMap :: Array Int CapEvent -> DbgMap
buildDbgMap arr = dbgMap
  where
    (imap, dbgMap) = go (BS.empty) 0 0 imap (IM.empty) (elems arr) []

    -- The somewhat complicated knot tying here is a bit silly, given
    -- that at some point we want to have unique instrumentation IDs
    -- anyway. However, as currently the offset stuff doesn't seem to work,
    -- we work around that by building one IntMap per module.

    go _     _     _ _     iMapO []     xs = (iMapO, xs)
    go mfile moff !i iMapI iMapO (e:es) xs = case spec $ ce_event e of
      CreateThread {}
        -> (iMapO, xs) -- Don't expect any further debug data
      DebugModule { file }
        -> let res = go file moff i (fst res) IM.empty es xs
           in (iMapO, snd res)
      DebugProcedure { instr, parent, label }
        -> let (!srcs, !ranges, !core) = go_proc [] [] Nothing es
               p_entry = parent >>= flip IM.lookup iMapI . fI
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
                                   , dbgParent = p_entry
                                   , dbgSources = srcs
                                   , dbgDCore = core
                                   }
               !iMapO' = case instr of
                 Just ix -> IM.insert (fI ix) entry iMapO
                 Nothing -> iMapO
           in go mfile moff (i+1) iMapI iMapO' es (entry:xs)
      _other -> go mfile moff i iMapI iMapO es xs

    go_proc srcs ranges core [] = (reverse srcs, ranges, core)
    go_proc srcs ranges core (e:es) = case spec (ce_event e) of
      DebugSource { sline, scol, eline, ecol, file, name=name' }
        -> let !span = Span file name' (fI sline) (fI scol) (fI eline) (fI ecol)
           in go_proc (span:srcs) ranges core es
      DebugPtrRange { low, high }
        -> go_proc srcs (IPRange (fromIntegral low) (fromIntegral high):ranges) core es
      DebugCore { coreBind, coreCons, coreCode }
        | not $ isJust core
        -> let core' = DebugEntryCore coreBind coreCons (getCoreExpr coreCode)
           in go_proc srcs ranges (Just core') es
      DebugProcedure {} -> stop
      CreateThread {} -> stop
      _other
        -> go_proc srcs ranges core es
      where stop = (reverse srcs, ranges, core)

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
      down []     []              !rm = rm
      down (s:ss) []              !rm = up ss []     (high s) rm
      down []     (r:rs)          !rm = down [r] rs           (IM.insert (low r) (dbg r) rm)
      down (s:ss) (r:rs)          !rm
        | high s <= low r             = up ss (r:rs) (high s) rm
        | otherwise                   = down (r:s:ss) rs      (IM.insert (low r) (dbg r) rm)

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
  = case IM.splitLookup ip $ rangeMap maps of
    (_, Just r, _)           -> Just r
    (lowerRanges, _, _)
      | IM.null lowerRanges  -> trace msg $ Nothing
      | (_, r) <- IM.findMax lowerRanges
        , any (\(IPRange l h) -> l <= ip && ip < h) (dbgRanges r)
                             -> Just r
      | otherwise            -> trace msg $ Nothing

  where msg = printf "Could not place IP %08x" ip

-- | Looks up a number of ranges, groups together same ranges
lookupRanges :: DebugMaps -> [Word64] -> [(Int, Maybe DebugEntry)]
lookupRanges maps ips =
  let -- Check whether the first IP has a match in the range map
      go []      _  res = res
      go (ip:is) rm res = case IM.splitLookup ip rm of
        (_,     Just d,  rm')         -> found 0 (ip:is) d rm' res
        (lower, Nothing, rm')
          | IM.null lower             -> not_found ip is rm' res
          | (_,d) <- IM.findMax lower -> found 0 (ip:is) d rm' res
          | otherwise                 -> not_found ip is rm' res
      -- Found a (possibly) matching debug entry, count number of IPs
      -- that actually match, then continue with reduced IPs and range
      -- map as well as another entry in the hits list..
      found n (ip:is) d rm res
        | any (\(IPRange l h) -> l <= ip && ip < h) (dbgRanges d)
          = found (n+1) is d rm res
      found n ips d rm (hs,ms)
          = go ips rm (if n > 0 then (n,Just d):hs else hs, ms)
      not_found ip is rm (hs, ms)
        = trace (printf "Could not place IP %08x" ip) $
          go is rm (hs, ms+1)

      (hits, misses) = go (sort $ map fromIntegral ips) (rangeMap maps) ([], 0)
   in if misses > 0
      then (misses, Nothing):hits
      else hits

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
-- Utils

bsToStr :: BS.ByteString -> String
bsToStr = map (chr.fromIntegral) . BS.unpack

strToBS :: String -> BS.ByteString
strToBS = BS.pack . map (fromIntegral.ord)

