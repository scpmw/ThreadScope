
{-# LANGUAGE StandaloneDeriving #-}

module GUI.SourceView (
  SourceView,
  sourceViewNew,
  SourceViewActions(..),

  Tag,

  sourceViewSetEvents,
  --sourceViewSelectTag,

  sourceViewSetCursor,
  ) where

import GHC.RTS.Events

import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView hiding (SourceView, sourceViewNew)
import qualified Graphics.UI.Gtk.SourceView as GtkSourceView

import Data.Array
import Data.IORef
import Data.Maybe (mapMaybe, catMaybes, fromMaybe)
import Data.List
import Data.Word (Word32, Word64)
import qualified Data.Function as F
import qualified Data.IntMap as IM
import Data.Char (isSpace)

import System.FilePath
import System.Directory (doesFileExist,getCurrentDirectory,canonicalizePath)

import Control.Monad (forM_, when)
import Control.Applicative ((<$>))
import Control.Arrow (first, second)

import Numeric (showHex)

import Text.Printf

import Paths_threadscope (getDataFileName)

import Debug.Trace

-------------------------------------------------------------------------------

data SourceView = SourceView {
  stateRef     :: !(IORef SourceViewState),
  sourceView   :: !GtkSourceView.SourceView,
  sourceBuffer :: !SourceBuffer,
  tagsStore    :: !(ListStore Tag),
  tagsTreeView :: !TreeView
  }

data DebugEntry = DebugEntry {
  dbgId :: {-# UNPACK #-} !Int, -- for quick identification checks
  dbgUnit :: String,
  dbgLabel :: String,
  dbgRanges :: [(Int, Int)],
  dbgDName :: Maybe String,
  dbgInstr :: Maybe Int,
  dbgParent :: Maybe DebugEntry,
  dbgSources :: [(Int, Int, Int, Int, String)],
  dbgDCore :: Maybe (String, String)
  } deriving Show

type EventsArray = Array Int CapEvent
type UnitMap = [(FilePath, String)]
type RangeMap = IM.IntMap DebugEntry
type DbgMap = [DebugEntry]

data Tag = Tag {
  -- | Compilation unit this tag belongs to
  tagUnit :: Maybe String,
  -- | Name of the tag. If @Nothing@, the tag does not have a name, as
  -- will be the case for most Haskell expressions
  tagName :: Maybe String,
  -- | Tick number of the tag. This is only set if there's a Haskell
  -- expression the tag could be mapped to
  tagTick :: Maybe Int,
  -- | Debug data available for the tag
  tagDebug :: Maybe DebugEntry,
  -- | Approximate frequency of the tag getting hit
  tagFreq :: !Double
  } deriving (Show)

instance Eq Tag where
  (Tag m1 n1 t1 _ _) == (Tag m2 n2 t2 _ _)  = m1 == m2 && (n1 == n2 || t1 == t2)

data SourceViewState
  = StateEmpty
  | StateLoaded {
    eventsArr  :: EventsArray,
    unitMap    :: UnitMap,      -- ^ Maps compilation unit names (from ranges) to actual file names
    dbgMap     :: DbgMap,
    rangeMap   :: RangeMap,
    tags       :: [Tag],
    selection  :: Maybe Tag,
    currentUnit:: Maybe String
  }

-- | Prefix for unknown compilation units - where the only reference
-- we could come up with came from the symbol table of a binary file
-- (which, sadly, is not enough to reliably locate source files even
-- with FILE entries)
symTabPrefix :: String
symTabPrefix = "SYMTAB: "

data SourceViewActions = SourceViewActions {
  --sourceViewRedraw :: IO ()
  }

sourceViewNew :: Builder -> SourceViewActions -> IO SourceView
sourceViewNew builder SourceViewActions{..} = do
  stateRef <- newIORef StateEmpty

  let getWidget cast = builderGetObject builder cast
  sourceView   <- getWidget castToSourceView "source"
  langManager  <- sourceLanguageManagerGetDefault
  searchPath   <- sourceLanguageManagerGetSearchPath langManager
  searchDir    <- getDataFileName "haskell.lang"

  -- Set monospace font for source view
  fontDesc     <- fontDescriptionNew
  fontDescriptionSetFamily fontDesc "Monospace"
  widgetModifyFont sourceView (Just fontDesc)

  -- Create source buffer using Haskell language specs
  sourceLanguageManagerSetSearchPath langManager
    (Just (takeDirectory searchDir : searchPath))
  haskellLang  <- sourceLanguageManagerGetLanguage langManager "haskell"
  sourceBuffer <- case haskellLang of
    Just haskell -> sourceBufferNewWithLanguage haskell
    Nothing      -> sourceBufferNew Nothing 
  textViewSetBuffer sourceView sourceBuffer

  -- Create columns for tag list
  tagsTreeView <- getWidget castToTreeView "source_tagstree"
  tagsStore    <- listStoreNew []

  let mkColumn title = do
        col <- treeViewColumnNew
        render <- cellRendererTextNew
        treeViewColumnSetTitle col title
        treeViewColumnPackStart col render True
        treeViewAppendColumn tagsTreeView col
        return (col, render)

  (tagFreqCol, tagFreqRender) <- mkColumn "%"
  (tagModCol, tagModRender)   <- mkColumn "Module"
  (tagDescCol, tagDescRender) <- mkColumn "Name"
  (tagCoreCol, tagCoreRender) <- mkColumn "Core"

  -- Set column content
  treeViewSetModel tagsTreeView tagsStore

  cellLayoutSetAttributes tagFreqCol tagFreqRender tagsStore $ \Tag{..} ->
    [ cellText := printf "%02.1f" (tagFreq * 100) ]
  --cellLayoutSetAttributes tagTickCol tagTickRender tagsStore $ \Tag{..} ->
  --  [ cellText := maybe "" show tagTick ]

  -- Go up in tree until element was found

  cellLayoutSetAttributes tagCoreCol tagCoreRender tagsStore $ \Tag{..} ->
    [ cellText := fromMaybe "" (fst <$> (findDbgElem dbgDCore tagDebug)) ]
  cellLayoutSetAttributes tagModCol tagModRender tagsStore $ \Tag{..} ->
    [ cellText :=
      let -- If the compilation unit is "SYMTAB", this is a pseudo
          -- module inserted by GHC for procedure ranges it found but
          -- couldn't map to a piece of Haskell code. We show the name
          -- of the binary the symbol came from in that case.
      in case fmap dbgUnit tagDebug of
            Just m | symTabPrefix `isPrefixOf` m
                     -> "(" ++ takeFileName (drop (length symTabPrefix) m) ++ ")"
                   | otherwise
                     -> takeFileName m
            Nothing  -> "(?)"
    ]
  cellLayoutSetAttributes tagDescCol tagDescRender tagsStore $ \Tag{..} ->
    [ cellText := case findDbgElem dbgDName tagDebug of
          -- We either get the name from an annotation, or we use the
          -- label used for the procedure in the binary symbol table
         Just n -> n
         _ | Just l <- fmap dbgLabel tagDebug
                -> "(" ++ zdecode l ++ ")"
           | otherwise
                -> "(?)"
    ]

  let srcView    = SourceView {..}

  -- Register events
  on tagsTreeView cursorChanged $
    updateTagSelection srcView

  on tagsTreeView rowActivated $ \_ _ -> do
    showCore srcView

  return srcView

sourceViewSetEvents :: SourceView -> Maybe FilePath -> Maybe EventsArray -> IO ()
sourceViewSetEvents SourceView{..} m_file m_data = do
  writeIORef stateRef =<< case (m_file, m_data) of
    (Just file, Just eventsArr) -> do

      -- Search dirs
      curDir <- getCurrentDirectory
      searchDirs <- nub <$> mapM canonicalizePath [takeDirectory file, curDir]

      -- Load debug data from event log
      let dbgMap = buildDbgMap eventsArr

      -- Build range map
      let rangeMap = buildRangeMap dbgMap
      putStrLn $ printf "Range map has %d entries" (IM.size rangeMap)

      -- Find source files
      let units = nub $
                  [ f | DebugEntry { dbgSources } <- dbgMap
                      , (_,_,_,_,f) <- dbgSources ] ++
                  [ f | DebugEntry { dbgUnit = f } <- dbgMap ]
      unitMap <- findSourceFiles searchDirs units
      mapM print unitMap
      putStrLn $ printf "Located %d out of %d source files" (length unitMap) (length units)

      let tags = tagsFromLocalTicks 0 eventsArr dbgMap ++
                 tagsFromLocalIPs2 0 eventsArr rangeMap
          selection = Nothing
          currentUnit = Nothing

      return StateLoaded {..}

    _other -> return StateEmpty

------------------------------------------------------------------------------

searchGen :: (FilePath -> IO Bool) -> [FilePath] -> FilePath -> IO (Maybe FilePath)
searchGen _    []         _    = return Nothing
searchGen test (dir:dirs) name = do
  full_name <- catch (canonicalizePath (combine dir name))
               (\_ -> return name)
  ex <- test full_name
  if ex
    then return (Just full_name)
    else searchGen test dirs name

searchFile{-, searchDirectory-} :: [FilePath] -> FilePath -> IO (Maybe FilePath)
searchFile = searchGen doesFileExist
--searchDirectory = searchGen doesDirectoryExist

findSourceFiles :: [FilePath] -> [FilePath] -> IO UnitMap
findSourceFiles searchDirs sources = catMaybes <$> mapM findSource sources
 where findSource source = ((,) source <$>) <$> searchFile searchDirs source

------------------------------------------------------------------------------

sourceViewSetCursor :: SourceView -> Int -> IO ()
sourceViewSetCursor view@SourceView {..} n = do
  state <- readIORef stateRef
  case state of
    StateLoaded{..} -> do

      -- Load tags for new position
      let n' = clampBounds (bounds eventsArr) n
          tags' = tagsFromLocalTicks n' eventsArr dbgMap ++
                  tagsFromLocalIPs2 n' eventsArr rangeMap

      -- Update selection, if possible
      let selection' = selection >>= (\t -> find (== t) tags')

      -- Set new state
      writeIORef stateRef state{ tags=tags', selection=selection' }

      -- Update views
      updateTagsView tagsStore tags'
      setTagSelection view selection'

    _ -> clearAll view

clearAll :: SourceView -> IO ()
clearAll SourceView{..} = do
  listStoreClear tagsStore
  textBufferSetText sourceBuffer ""

updateTagsView :: ListStore Tag -> [Tag] -> IO ()
updateTagsView tagsStore tags = do
  listStoreClear tagsStore
  let subsumed = subsumeTags tags
      sorted = sortBy (flip (compare `F.on` tagFreq)) subsumed
  forM_ sorted $ \tag -> do
    listStoreAppend tagsStore tag

setTagSelection :: SourceView -> Maybe Tag -> IO ()
setTagSelection SourceView{..} m_tag = do
  tagSelect <- treeViewGetSelection tagsTreeView
  tagsShown <- listStoreToList tagsStore
  case flip elemIndex tagsShown =<< m_tag of
    Just ix -> treeSelectionSelectPath tagSelect [ix]
    Nothing -> treeSelectionUnselectAll tagSelect

updateTagSelection :: SourceView -> IO ()
updateTagSelection view@SourceView{..} = do
  state <- readIORef stateRef
  tagSelect <- treeViewGetSelection tagsTreeView
  case state of
    StateLoaded{..} -> do
      m_iter <- treeSelectionGetSelected tagSelect
      select' <- case m_iter of
        Just iter -> Just <$> listStoreGetValue tagsStore (listStoreIterToIndex iter)
        Nothing   -> return Nothing
      writeIORef stateRef state{
        selection = select'
        }
      -- Update view
      case tagUnit =<< select' of
        Just unit -> do
          showUnit view unit
          updateTextTags view
        Nothing -> return ()
    _ -> return ()

------------------------------------------------------------------------------

findLocalEvents :: Int -> Timestamp -> EventsArray -> (CapEvent -> Maybe a) -> [a]
findLocalEvents n winSize eventsArr filter_f =
  let winMid        = time $ ce_event $ eventsArr ! n
      eventsWhile f = takeWhile (f.time.ce_event) . map (eventsArr !)
      (low, high)   = bounds eventsArr
      eventsBefore  = eventsWhile (>winMid-winSize) [n-1,n-2..low]
      eventsAfter   = eventsWhile (<winMid+winSize) [n,n+1..high]
  in mapMaybe filter_f eventsBefore ++ mapMaybe filter_f eventsAfter

findLocalEventsCount :: Int -> Int -> EventsArray -> (CapEvent -> Maybe a) -> (a -> Int) -> [a]
findLocalEventsCount n maxCount eventsArr filter_f count_f =
  let winMid        = time $ ce_event $ eventsArr ! n
      midDist e     = absDiff (time (ce_event e)) winMid
      absDiff x y  = if x > y then x - y else y - x

      -- Get events before and after point of interest
      (low, high)   = bounds eventsArr
      eventsBefore  = map (eventsArr !) [n-1,n-2..low]
      eventsAfter   = map (eventsArr !) [n,n+1..high]

      -- Combine events into a common list, sorted by distance to mid point
      zippity [] as = as
      zippity bs [] = bs
      zippity (a:as) (b:bs)
        | midDist a < midDist b
                    = a:zippity as (b:bs)
        | otherwise = b:zippity (a:as) bs
      zippedEvents  = zippity eventsBefore eventsAfter

      -- Filter the events
      filteredEvents= mapMaybe filter_f zippedEvents

      -- Accumulate using count_f how many samples each event
      -- has. Stop once we have enough. Note that scanl adds the
      -- initial accumulator to the list, so we actually get the index
      -- of the first element where we go over the count.
      returnCount   = findIndex (>maxCount) $
                      scanl (\c x -> c + count_f x) 0 filteredEvents

  in case returnCount of
    Just ix   -> take ix filteredEvents
    Nothing   -> filteredEvents

-- Dumps are normally generated for every RTS timer event, which fires
-- 50 times per second (= 20 ms).
tickSampleWinSize, tickSampleStdDev :: Timestamp
tickSampleWinSize = 50 * 1000 * 1000
tickSampleStdDev  = 20 * 1000 * 1000

findLocalTickSamples :: Int -> EventsArray -> [(Timestamp, [(Word32, Word32)])]
findLocalTickSamples startIx eventsArr =
  let getTick CapEvent { ce_event = Event { time, spec = TickDump{..} } }
        = Just ({-cap, -}time, dump)
      getTick _other
        = Nothing
  in findLocalEvents startIx tickSampleWinSize eventsArr getTick

-- | Weight to give to a sample. Simply the normal distribution.
sampleWeight :: Timestamp -> Timestamp -> Timestamp -> Double
sampleWeight mid dev val = 
  exp (fromIntegral ((val - mid) ^ 2) / fromIntegral (dev * dev))

weightTicks :: Timestamp -> Timestamp -> [(Word32, Word32)] -> [(Word32, Double)]
weightTicks mid time = map (second f)
  where f hitCount = fromIntegral hitCount * weight
        weight     = sampleWeight mid tickSampleStdDev time

findLocalTicks :: Int -> EventsArray -> [(Word32, Double)]
findLocalTicks startIx eventsArr =

  -- Find ticks around start point
  let winMid        = time $ ce_event $ eventsArr ! startIx
      samples       = findLocalTickSamples startIx eventsArr

      -- Weight them by distance to start point
      weighted      = map (uncurry $ weightTicks winMid) samples

      -- Sum up weighted frequencies by tick IDs
      summed        = nubSumBy (compare `F.on` fst) (\(t,w1) (_,w2)->(t,w1+w2)) $ concat weighted
      grandSum      = sum $ map snd summed
      normalized    = map (second (/ grandSum)) summed

  in normalized

tagsFromLocalTicks :: Int -> EventsArray -> DbgMap -> [Tag]
tagsFromLocalTicks startIx eventsArr dbgMap = map toTag $ findLocalTicks startIx eventsArr
  where toTag (tick, freq) =
          let dbg = lookupDbgInstr (fromIntegral tick) dbgMap
          in Tag { tagUnit   = dbgUnit <$> dbg
                 , tagName   = Nothing
                 , tagTick   = Just (fromIntegral tick)
                 , tagDebug  = dbg
                 , tagFreq   = freq
                 }

-------------------------------------------------------------------------------

-- Instruction pointer samples are a bit more tricky than tick
-- samples. Let's assume one sample every 10,000 cycles (current
-- default) and one event per 1,024 samples (ditto), this makes about
-- 10,000,000 cycles per event. As modern computers run at clock speed
-- of at least 1,000,000,000 Hz, this means events should be about
-- 10ms apart, at maximum.
--
-- Obviously this argument isn't really water-proof, as our profiling
-- eats away at this as well. Sooner or later we might want something
-- adaptive...
ipSampleWinSize, ipSampleStdDev :: Timestamp
ipSampleWinSize = 50 * 1000 * 1000
ipSampleStdDev  = 20 * 1000 * 1000
ipSampleMaxCount :: Int
ipSampleMaxCount = 1000

-- | Lookup an instruction pointer in the range map.
lookupRange :: RangeMap -> Int -> Maybe DebugEntry
lookupRange rangeMap ip
  = case IM.splitLookup ip rangeMap of
    (_, Just r, _)           -> Just r
    (lowerRanges, _, _)
      | IM.null lowerRanges  -> trace msg $ Nothing
      | (_, r) <- IM.findMax lowerRanges
        , any (\(l, h) -> l <= ip && ip < h) (dbgRanges r)
                             -> Just r
      | otherwise            -> trace msg $ Nothing

  where msg = printf "Could not place IP %08x" ip

buildRangeMap :: [DebugEntry] -> IM.IntMap DebugEntry
buildRangeMap dbgs =
  let -- Convert input into a tuple list with one entry per range. Sort.
      ranges = [ (fromIntegral l, fromIntegral h, dbg)
               | dbg <- dbgs, (l, h) <- dbgRanges dbg ]
      low  (l,_,_) = l
      high (_,h,_) = h
      dbg  (_,_,d) = d
      sorted = sortBy (compare `F.on` low) ranges

      -- Scans range list and inserts all ranges into a map.
      down :: [(Int, Int, DebugEntry)] -> [(Int, Int, DebugEntry)]
              -> [(Int, DebugEntry)]
      down []     []              = []
      down (s:ss) []              =                   up ss []     (high s)
      down []     (r:rs)          = (low r, dbg r) : (down [r] rs)
      down (s:ss) (r:rs)
        | high s <= low r         =                   up ss (r:rs) (high s)
        | otherwise               = (low r, dbg r) : (down (r:s:ss) rs)

      -- Called to remove items from the stack, maybe re-inserting
      -- some ranges that were overriden but still apply. Will revert
      -- to "down" behaviour once no more ranges can be popped from
      -- the stack.
      up :: [(Int, Int, DebugEntry)] -> [(Int, Int, DebugEntry)] -> Int
            -> [(Int, DebugEntry)]
      up []     rs _   =               down [] rs
      up (s:ss) rs p
        | high s > p   = (p, dbg s) : (down (s:ss) rs)
        | otherwise    =               up   ss rs p

  in IM.fromAscList (down [] sorted)

findLocalIPsamples :: Int -> EventsArray -> [(Timestamp, [Word64])]
findLocalIPsamples startIx eventsArr =
  let getIPs CapEvent{{-ce_cap,-}ce_event=Event{time,spec=InstrPtrSample{..}}}
        = Just ({- ce_cap, -}time, ips)
--      getIPs CapEvent{{-ce_cap,-}ce_event=Event{time,spec=Blackhole{..}}} 
--        = Just (time, [ip])
      getIPs _other
        = Nothing
      count_f (_, ips) = length ips
      samples = findLocalEventsCount startIx ipSampleMaxCount eventsArr getIPs count_f
  in traceShow (map (second length) samples) samples

-- | Looks up a number of ranges, groups together same ranges
lookupRanges :: RangeMap -> [Word64] -> [(Int, Maybe DebugEntry)]
lookupRanges rangeMap ips =
  let ranges = map (lookupRange rangeMap . fromIntegral) ips
      ord = compare `F.on` (fmap dbgId . snd)
      _ `plus` (n, r) = (n+1, r)
  in nubSumBy ord plus $ map ((,) 1) ranges

-- | Removes duplicates and allows for custom combination function
nubSumBy :: (a -> a -> Ordering) -> (a -> a -> a) -> [a] -> [a]
nubSumBy ord plus = map (foldr1 plus) . groupBy eq . sortBy ord
  where x `eq` y = ord x y == EQ

-- | Finds local IP samples, return weighted
findLocalIPsWeighted :: Int -> EventsArray -> RangeMap -> [(Double, Maybe DebugEntry)]
findLocalIPsWeighted startIx eventsArr rangeMap =
  let -- Find samples
      ipss = findLocalIPsamples startIx eventsArr

      -- Give each range a weight
      winMid = time $ ce_event $ eventsArr ! startIx
      weight = sampleWeight winMid ipSampleStdDev
      toWeighted t = first ((* weight t) . fromIntegral)
      worker (t, ips) = map (toWeighted t) $ lookupRanges rangeMap ips
      wips = concatMap worker ipss

      -- Combine duplicated ranges
      ord = compare `F.on` (fmap dbgId . snd)
      (w1, _) `plus` (w2, r) = (w1+w2, r)
  in nubSumBy ord plus wips

tagsFromLocalIPs2 :: Int -> EventsArray -> RangeMap -> [Tag]
tagsFromLocalIPs2 startIx eventsArr rangeMap =
  let toTag freq (Just dbg@DebugEntry{..})
        = Tag { tagUnit = Just dbgUnit
              , tagName = Just $ zdecode dbgLabel
              , tagTick = dbgInstr
              , tagDebug = Just dbg
              , tagFreq = freq
              }
      toTag freq Nothing
        = Tag { tagUnit = Nothing
              , tagName = Just "(unrecognized)"
              , tagTick = Nothing
              , tagDebug = Nothing
              , tagFreq = freq
              }

      weighted = findLocalIPsWeighted startIx eventsArr rangeMap
      grandSum = sum $ map fst weighted
      tags = map (uncurry toTag . first (/grandSum)) weighted

      ord = compare `F.on` (fmap dbgId . tagDebug)
      t1 `plus` t2 = t1 { tagFreq = tagFreq t1 + tagFreq t2 }
  in nubSumBy ord plus tags

zdecode :: String -> String
zdecode ('z':'a':cs) = '&':zdecode cs
zdecode ('z':'b':cs) = '|':zdecode cs
zdecode ('z':'c':cs) = '^':zdecode cs
zdecode ('z':'d':cs) = '$':zdecode cs
zdecode ('z':'e':cs) = '=':zdecode cs
zdecode ('z':'g':cs) = '>':zdecode cs
zdecode ('z':'h':cs) = '#':zdecode cs
zdecode ('z':'i':cs) = '.':zdecode cs
zdecode ('z':'l':cs) = '<':zdecode cs
zdecode ('z':'m':cs) = '-':zdecode cs
zdecode ('z':'n':cs) = '!':zdecode cs
zdecode ('z':'p':cs) = '+':zdecode cs
zdecode ('z':'q':cs) = '\'':zdecode cs
zdecode ('z':'r':cs) = '\\':zdecode cs
zdecode ('z':'s':cs) = '/':zdecode cs
zdecode ('z':'t':cs) = '*':zdecode cs
zdecode ('z':'u':cs) = '_':zdecode cs
zdecode ('z':'v':cs) = '%':zdecode cs
zdecode ('z':'z':cs) = 'z':zdecode cs
zdecode (c:cs)       = c:zdecode cs
zdecode []           = []

-------------------------------------------------------------------------------

clearTextTags :: SourceBuffer -> IO ()
clearTextTags sourceBuffer = do
{-  tagListRef <- newIORef []
  tagTable <- textBufferGetTagTable sourceBuffer
  -- textTagTableForeach tagTable (textTagTableRemove tagTable)  
  textTagTableForeach tagTable (\t -> modifyIORef tagListRef (t:))
  tagList <- readIORef tagListRef
  mapM_ (textTagTableRemove tagTable) tagList-}
  return ()

showUnit :: SourceView -> String -> IO ()
showUnit view@SourceView{..} unit = do
  state <- readIORef stateRef
  clearTextTags sourceBuffer

  case state of
    StateLoaded{..}
      | Just file <- lookup unit unitMap ->
        when (currentUnit /= Just unit) $ do

          -- Load the source file
          modSrc <- readFile file
          textBufferSetText sourceBuffer modSrc

          -- Set state
          writeIORef stateRef state{ currentUnit = Just unit }

          -- Update tagging
          updateTextTags view

    -- We only have a symbol name and the file name to the binary?
    -- That's expected for binaries without debug info. Explain this
    -- to the user.
    StateLoaded{selection} | Just Tag{tagName = Just n} <- selection
                           , symTabPrefix `isPrefixOf` unit -> do
      writeIORef stateRef state{ currentUnit = Nothing }
      textBufferSetText sourceBuffer $
        " *** Symbol from binary file ***\n\n" ++
        "No source code info could be found...\n" ++
        "This code was probably compiled without debugging annotations.\n\n" ++
        "Binary: " ++ drop (length symTabPrefix) unit ++ "\n" ++
        "Symbol: " ++ n

    StateLoaded{} -> do
      writeIORef stateRef state{ currentUnit = Nothing }
      textBufferSetText sourceBuffer $
        " *** Could not find source code for " ++ unit ++ "! ***"

    StateEmpty -> do
      writeIORef stateRef state{ currentUnit = Nothing }
      textBufferSetText sourceBuffer ""

updateTextTags :: SourceView -> IO ()
updateTextTags SourceView{..} = do

  state <- readIORef stateRef
  case state of
    StateLoaded{..} -> do

        -- Clear existing tags
        clearTextTags sourceBuffer

        -- Annotate source code
        let filterLocal = filter ((== currentUnit) . tagUnit)
        annotateTags sourceView sourceBuffer (filterLocal tags) False
        case selection of
         Nothing  -> return ()
         Just sel -> annotateTags sourceView sourceBuffer (filterLocal [sel]) True

    _ -> return ()

annotateTags :: GtkSourceView.SourceView -> SourceBuffer -> [Tag] -> Bool -> IO ()
annotateTags sourceView sourceBuffer tags sel = do
  tagTable <- textBufferGetTagTable sourceBuffer

  let freqMap2 = [(tagFreq, (1, srcs))
                 | Tag {tagFreq, tagDebug = Just dbg} <- tags
                 , srcs <- extSources dbg ]
      extSources DebugEntry { dbgSources, dbgDName = Nothing, dbgParent = Just p}
        = dbgSources ++ extSources p
      extSources DebugEntry { dbgSources}
        = dbgSources
      freqMap'  = nubSumBy (compare `F.on` snd) (\(f1, _) (f2, t) -> (f1+f2,t)) 
                  freqMap2

  -- "Safe" version of textBufferGetIterAtLineOffset
  lineCount <- textBufferGetLineCount sourceBuffer
  let textBufferGetIterAtLineOffsetSafe l c
        | l >= lineCount = textBufferGetEndIter sourceBuffer
        | otherwise = do iter <- textBufferGetIterAtLine sourceBuffer l
                         textIterForwardChars iter c
                         return iter

  forM_ freqMap' $ \(freq, (lvl, (sl, sc, el, ec, _))) -> when (sel || lvl == 1) $ do

    -- Create color tag
    tag <- textTagNew Nothing
    let w = min 230 $ max 0 $ 255 - round (100 * freq)
        ws w | w < 16 = '0': showHex w ""
             | True   = showHex w ""
        --w' = 255 - round (fromIntegral 255 / fromIntegral (-lvl))
        clr | sel  = "#8888ff"
            | True = "#" ++ ws w ++ ws w ++ ws w
    set tag [textTagBackground := clr, textTagBackgroundSet := True]
    tagTable `textTagTableAdd` tag

      -- Place at code position
    start <- textBufferGetIterAtLineOffsetSafe (sl-1) (sc-1)
    end <- textBufferGetIterAtLineOffsetSafe (el-1) ec
    textBufferApplyTag sourceBuffer tag start end

    --putStrLn $ "Annotating " ++ show (sl, sc, el, ec) ++ " with " ++ clr ++ "(lvl " ++ show lvl ++ ")"

  -- Scroll to largest tick
  let ticks = map (snd . snd) freqMap'
  when (sel && not (null ticks)) $ do
    let hpcSize (sl, sc, el, ec, _) = (el-sl, ec-sc)
        largestTick = maximumBy (compare `F.on` hpcSize) ticks
        (l, c, _, _, _) = largestTick
    iter <- textBufferGetIterAtLineOffset sourceBuffer (l-1) (c-1)
    _ <- textViewScrollToIter sourceView iter 0.2 Nothing
    return ()

-------------------------------------------------------------------------------

showCore :: SourceView -> IO ()
showCore SourceView{..} = do
  state <- readIORef stateRef

  case state of
    StateLoaded{..}
      | Just tag <- selection
      , Just core <- fmap snd (findDbgElem dbgDCore (tagDebug tag)) -> do

        clearTextTags sourceBuffer
        textBufferSetText sourceBuffer (cleanupCore core)

        writeIORef stateRef state { currentUnit = Nothing }

    _ -> return ()

cleanupCore :: String -> String
cleanupCore = unlines . go . lines
  where
    go (l1:l2:ls) | trivialBlank l1
                  , trivialBlank l2
                  , takeWhile isSpace l1 == takeWhile isSpace l2
                  , length l1 < 80
      = go ((l1 ++ " " ++ dropWhile isSpace l2):ls)
    go (l:ls) = l:go ls
    go []     = []

    trivialBlank ""       = True
    trivialBlank (' ':xs) = trivialBlank xs
    trivialBlank ('}':xs) = trivialBlank xs
    trivialBlank (')':xs) = trivialBlank xs
    trivialBlank (';':xs) = trivialBlank xs
    trivialBlank _        = False


-------------------------------------------------------------------------------

-- This *really* ought to be in a standard library somwhere.
clampBounds :: Ord a => (a, a) -> a -> a
clampBounds (lower, upper) x
  | x <= lower = lower
  | x >  upper = upper
  | otherwise  = x

-------------------------------------------------------------------------------

lookupDbgInstr :: Int -> DbgMap -> Maybe DebugEntry
lookupDbgInstr instr = find ((== Just instr) . dbgInstr)

buildDbgMap :: EventsArray -> DbgMap
buildDbgMap arr = dbgMap
  where
    (imap, dbgMap) = go "" 0 0 imap (IM.empty) (elems arr) []

    go _     _     _ _     iMapO []     xs = (iMapO, xs)
    go mfile moff !i iMapI iMapO (e:es) xs = case spec $ ce_event e of
      CreateThread {}
        -> (iMapO, xs) -- Don't expect any further debug data
      DebugModule { file }
        -> let res = go file moff i (fst res) IM.empty es xs
           in (iMapO, snd res)
      DebugProcedure { instr, parent, label }
        -> let (name, srcs, ranges, core) = go_proc Nothing [] [] Nothing es
               p_entry = parent >>= flip IM.lookup iMapI . fI
               entry = DebugEntry { dbgId = i
                                  , dbgUnit = mfile
                                  , dbgLabel = label
                                  , dbgRanges = ranges
                                  , dbgDName = name
                                  , dbgInstr = fmap (fI.(+moff).fI) instr
                                  , dbgParent = p_entry
                                  , dbgSources = srcs
                                  , dbgDCore = core
                                  }
               iMapO' = case instr of
                 Just ix -> IM.insert (fI ix) entry iMapO
                 Nothing -> iMapO
           in go mfile moff (i+1) iMapI iMapO' es (entry:xs)
      _other -> go mfile moff i iMapI iMapO es xs

    go_proc name srcs ranges core [] = (name, reverse srcs, ranges, core)
    go_proc name srcs ranges core (e:es) = case spec $ ce_event e of
      DebugSource { sline, scol, eline, ecol, file }
        -> go_proc name ((fI sline, fI scol, fI eline, fI ecol, file):srcs) ranges core es
      DebugPtrRange { low, high }
        -> go_proc name srcs ((fromIntegral low, fromIntegral high):ranges) core es
      DebugCore { coreBind, coreCode } | core == Nothing
        -> go_proc name srcs ranges (Just (coreBind, coreCode)) es
      DebugName { dbgName } -- | name == Nothing
        -> go_proc (Just dbgName) srcs ranges core es
      DebugProcedure {} -> stop
      CreateThread {} -> stop
      _other
        -> go_proc name srcs ranges core es
      where stop = (name, reverse srcs, ranges, core)

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

------------------------------------------------------------------------------

-- | Subsume tags for the viewed list
subsumeTags :: [Tag] -> [Tag]
subsumeTags = nubSumBy cmp plus
   where cmp = compare `F.on` (dbgCmpId . tagDebug)
         dbgCmpId dbg = case findWithDbgElem dbgDCore dbg of
           Just DebugEntry { dbgDCore = Just (n, _) }
                       -> Left n
           Nothing     -> Right (fmap dbgId dbg)
         t1 `plus` t2 = t1 { tagFreq = tagFreq t1 + tagFreq t2 }
