
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
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List
import Data.Word (Word32, Word64)
import qualified Data.Function as F
import qualified Data.IntMap as IM
import qualified Data.Map as Map
import Data.Char (isSpace, chr)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import System.FilePath
import System.Directory (doesFileExist,getCurrentDirectory,canonicalizePath)

import Control.Monad (forM_, when)
import Control.Applicative ((<$>), (<|>))
import Control.Arrow (first, second)
import Control.DeepSeq (deepseq, NFData(..))
import Control.Monad.IO.Class (liftIO)

import Text.Printf

import Paths_threadscope (getDataFileName)

import Debug.Trace

-------------------------------------------------------------------------------

data FileView = FileView {
  sourceFile     :: FilePath,
  sourceLoad     :: (IORef Bool),
  sourcePage     :: Int,
  sourceView     :: GtkSourceView.SourceView,
  sourceBuffer   :: SourceBuffer
  }

data SourceView = SourceView {
  stateRef     :: (IORef SourceViewState),
  sourceBook   :: Notebook,
  coreView     :: GtkSourceView.SourceView,
  coreBuffer   :: SourceBuffer,
  sourceFont   :: FontDescription,
  haskellLang  :: Maybe SourceLanguage,
  tagsTreeView :: TreeView,
  tagsStore    :: (ListStore Tag)
  }

data Span = Span {
  fileName    :: String,
  startLine   :: {-# UNPACK #-} !Int,
  startCol    :: {-# UNPACK #-} !Int,
  endLine     :: {-# UNPACK #-} !Int,
  endCol      :: {-# UNPACK #-} !Int
  } deriving (Eq, Ord)

data IPRange = IPRange {
  _rangeStart :: {-# UNPACK #-} !Int,
  _rangeEnd   :: {-# UNPACK #-} !Int
  }

data DebugEntry = DebugEntry {
  dbgId     :: {-# UNPACK #-} !Int, -- for quick identification checks
  dbgUnit   :: String,
  dbgLabel  :: String,
  dbgRanges :: [IPRange],
  dbgDName  :: Maybe String,
  dbgInstr  :: Maybe Int,
  dbgParent :: Maybe DebugEntry,
  dbgSources:: [Span],
  dbgDCore  :: Maybe (String, LBS.ByteString)
  }

instance Eq DebugEntry where
  e1 == e2  = dbgId e1 == dbgId e2

type EventsArray = Array Int CapEvent
type RangeMap = IM.IntMap DebugEntry
type DbgMap = [DebugEntry]
type CoreMap = Map.Map (String, String) DebugEntry

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
  }

instance Eq Tag where
  (Tag m1 n1 t1 _ _) == (Tag m2 n2 t2 _ _)  = m1 == m2 && (n1 == n2 || t1 == t2)

instance NFData Tag where
  rnf Tag{..} = tagUnit `deepseq` tagName `deepseq` tagTick `deepseq` tagDebug `seq` ()

data SourceViewState
  = SourceViewState {
    eventsArr  :: EventsArray,
    dbgMap     :: DbgMap,
    rangeMap   :: RangeMap,
    coreMap    :: CoreMap,
    tags       :: [Tag],
    selection  :: Maybe Tag,
    files      :: [FileView],
    searchDirs :: [FilePath],
    lineTags   :: [Tag]
  }

initViewState :: SourceViewState
initViewState = SourceViewState {
  eventsArr = listArray (0,0) [],
  dbgMap = [],
  rangeMap = IM.empty,
  coreMap = Map.empty,
  tags = [],
  selection = Nothing,
  files = [],
  searchDirs = [],
  lineTags = []
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

  let getWidget cast = builderGetObject builder cast
  sourceBook   <- getWidget castToNotebook "source_book"
  coreView     <- getWidget castToSourceView "core"
  langManager  <- sourceLanguageManagerGetDefault
  searchPath   <- sourceLanguageManagerGetSearchPath langManager
  searchDir    <- getDataFileName "haskell.lang"

  -- Set monospace font for source view
  sourceFont   <- fontDescriptionNew
  fontDescriptionSetFamily sourceFont "Monospace"
  widgetModifyFont coreView (Just sourceFont)

  -- Create source buffer using Haskell language specs
  sourceLanguageManagerSetSearchPath langManager
    (Just (takeDirectory searchDir : searchPath))
  haskellLang  <- sourceLanguageManagerGetLanguage langManager "haskell"
  coreBuffer <- case haskellLang of
    Just haskell -> sourceBufferNewWithLanguage haskell
    Nothing      -> sourceBufferNew Nothing
  textViewSetBuffer coreView coreBuffer

  -- Lookup mark icons
  sourceViewSetMarkCategoryIconFromStock coreView coreMarkCatFolded (Just stockAdd)
  sourceViewSetMarkCategoryIconFromStock coreView coreMarkCatOpen (Just stockRemove)

  -- Set up cost annotations for core view
  costRenderer <- cellRendererTextNew
  wdt <- getTextWidth coreView "100.0%"
  set costRenderer [ cellTextSingleParagraphMode := True
                   , cellTextBackgroundColor := Color 0xffff 0x0000 0x0000
                   , cellTextBackgroundSet := True
                   , cellTextEditable := False
                   , cellTextAlignment := AlignRight
                   , cellWidth := wdt
                   ]
  coreGutter <- sourceViewGetGutter coreView TextWindowLeft
  sourceGutterInsert coreGutter costRenderer (-10)
   -- sourceGutterSetCellSizeFunc coreGutter costRenderer 

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

  stateRef <- newIORef initViewState
  let srcView    = SourceView {..}

  -- Register events
  on tagsTreeView cursorChanged $
    updateTagSelection srcView
  on sourceBook switchPage $
    updateFileView srcView
  after coreView sourceViewLineMarkActivated $ \pos ->
    liftIO $ activateMark srcView coreBuffer pos

  bgColor <- flip styleGetBackground StateNormal =<< widgetGetStyle coreView
  sourceGutterSetCellDataFunc coreGutter costRenderer $ \_cell l _ -> do
    SourceViewState{lineTags} <- readIORef stateRef
    let (text, m) = case drop l lineTags of
          Tag{..}:_ -> (printf "%02.1f" (tagFreq * 100) ++ "%",
                        max 0 (1 + log tagFreq / 7))
          _         -> ("-", 0)
        mix m (Color r1 g1 b1) (Color r2 g2 b2)
          = Color (round $ m * fromIntegral r1 + (1-m) * fromIntegral r2)
                  (round $ m * fromIntegral g1 + (1-m) * fromIntegral g2)
                  (round $ m * fromIntegral b1 + (1-m) * fromIntegral b2)
        lColor = Color 0x0000 0xffff 0xffff
    set costRenderer [ cellText := text
                     , cellTextBackgroundColor := mix (m :: Double) lColor bgColor
                     ]

  return srcView

-- | Returns the width of the given string in pixel when rendered on
-- screen. There is bound to be a better way of doing this, but I
-- haven't been able to find it so far. Reserved so I can come back to
-- this later.
getTextWidth :: WidgetClass w => w -> String -> IO Int
getTextWidth widget str = do
  pango <- widgetGetPangoContext widget
  layout <- layoutText pango str
  (Rectangle _ _ wdt _, _) <- layoutGetPixelExtents layout
  return wdt

sourceViewSetEvents :: SourceView -> Maybe FilePath -> Maybe EventsArray -> IO ()
sourceViewSetEvents SourceView{..} m_file m_data = do
  writeIORef stateRef =<< case (m_file, m_data) of
    (Just file, Just eventsArr) -> do

      -- Search dirs
      curDir <- getCurrentDirectory
      searchDirs <- nub <$> mapM canonicalizePath [takeDirectory file, curDir]

      -- Load debug data from event log
      let dbgMap = buildDbgMap eventsArr
      putStrLn $ printf "Read debug data on %d procedures" (length dbgMap)

      -- Build range map
      let rangeMap = buildRangeMap dbgMap
      putStrLn $ printf "Range map has %d entries" (IM.size rangeMap)

      -- Build core map
      let coreMap = buildCoreMap dbgMap
      putStrLn $ printf "Core map has %d entries" (Map.size coreMap)

      -- Find initial tags
      let tags = tagsFromLocalTicks 0 eventsArr dbgMap ++
                 tagsFromLocalIPs2 0 eventsArr rangeMap
          selection = Nothing

      let files = []
          lineTags = []
      return SourceViewState {..}

    _other -> return initViewState

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

------------------------------------------------------------------------------

-- | Returns a file view for the given file. Automatically creates the
-- file view if it doesn't exist yet.
fileViewGet :: SourceView -> FilePath -> IO FileView
fileViewGet sourceView file = do

  -- Look it up in the file list
  SourceViewState {..} <- readIORef $ stateRef sourceView
  case filter ((== file) . sourceFile) files of
    (view:_) -> return view
    []       -> fileViewNew sourceView file

-- | Creates a source view tab for the given file.
fileViewNew :: SourceView -> FilePath -> IO FileView
fileViewNew SourceView { stateRef, haskellLang, sourceFont, sourceBook } sourceFile = do

  -- Create source buffer
  sourceBuffer <- case haskellLang of
    Just haskell -> sourceBufferNewWithLanguage haskell
    Nothing      -> sourceBufferNew Nothing

  -- Create view
  sourceView <- sourceViewNewWithBuffer sourceBuffer
  set sourceView
    [ textViewEditable := False
    , sourceViewShowLineNumbers := True
    ]
  widgetModifyFont sourceView (Just sourceFont)

  -- Create scrolled window to allow scrolling in view
  scroll     <- scrolledWindowNew Nothing Nothing
  containerAdd scroll sourceView

  -- Create notebook page for the file
  sourcePage <- notebookAppendPage sourceBook scroll $ takeFileName sourceFile

  -- Show everything
  widgetShowAll scroll

  -- We don't actually load any content yet
  sourceLoad <- newIORef False

  -- Add to file view list
  let fileView = FileView {..}
  modifyIORef stateRef $ \state ->
    state { files = files state ++ [fileView] }

  return fileView

showFile :: SourceView -> FileView -> IO ()
showFile sview fview@FileView{..} = do

  -- Text needs to be load in the first place?
  isLoad <- readIORef sourceLoad
  when (not isLoad) $ do

    -- Find the source file
    SourceViewState{searchDirs} <- readIORef $ stateRef sview
    r <- searchFile searchDirs sourceFile
    case r of
      Just file -> do
        -- Load the source file
        modSrc <- BS.readFile file
        textBufferSetByteString sourceBuffer modSrc
        writeIORef sourceLoad True

      Nothing ->
        textBufferSetText sourceBuffer $
          "Could not find source file " ++ sourceFile ++ "!"

  -- Update tags
  isLoad <- readIORef sourceLoad
  when isLoad $
    updateTextTags sview fview

updateFileView :: SourceView -> Int -> IO ()
updateFileView srcView@SourceView{stateRef} page = do

  -- Find page
  SourceViewState{files} <- readIORef stateRef
  case find ((== page) . sourcePage) files of
    Just fileView -> showFile srcView fileView
    Nothing       -> return ()

------------------------------------------------------------------------------

sourceViewSetCursor :: SourceView -> Int -> IO ()
sourceViewSetCursor view@SourceView {..} n = do
  state@SourceViewState{..} <- readIORef stateRef

  -- Load tags for new position
  let n' = clampBounds (bounds eventsArr) n
      tags' = tagsFromLocalTicks n' eventsArr dbgMap ++
              tagsFromLocalIPs2 n' eventsArr rangeMap

  -- Update selection, if possible
  let selection' = selection >>= (\t -> find (== t) tags')

  -- Set new state
  tags' `deepseq` writeIORef stateRef state{ tags=tags', selection=selection' }

  -- Update views
  postGUISync $ do
    updateTagsView tagsStore tags'
    updateLineTags view
    setTagSelection view selection'

updateTagsView :: ListStore Tag -> [Tag] -> IO ()
updateTagsView tagsStore tags = do
  let subsumed = subsumeTags tags
      sorted = sortBy (flip (compare `F.on` tagFreq)) subsumed
  listStoreClear tagsStore
  mapM_ (listStoreAppend tagsStore) sorted

setTagSelection :: SourceView -> Maybe Tag -> IO ()
setTagSelection SourceView{..} m_tag = do
  tagSelect <- treeViewGetSelection tagsTreeView
  tagsShown <- listStoreToList tagsStore
  case flip elemIndex tagsShown =<< m_tag of
    Just ix -> treeSelectionSelectPath tagSelect [ix]
    Nothing -> treeSelectionUnselectAll tagSelect

updateTagSelection :: SourceView -> IO ()
updateTagSelection view@SourceView{..} = do
  state@SourceViewState{..} <- readIORef stateRef

  -- Read selection from list view, write to state
  tagSelect <- treeViewGetSelection tagsTreeView
  m_iter <- treeSelectionGetSelected tagSelect
  select' <- case m_iter of
        Just iter -> Just <$> listStoreGetValue tagsStore (listStoreIterToIndex iter)
        Nothing   -> return Nothing
  writeIORef stateRef state {
    selection = select'
    }

  case select' of
    Just tag -> do

      -- Find files to show
      let units = nub $
                  (case tagUnit tag of
                      Just unit -> [unit]
                      Nothing -> []) ++
                  (case tagDebug tag of
                      Just dbg -> map fileName $ extSources dbg
                      Nothing  -> [])
      mapM_ (fileViewGet view) units

      -- Update labels
      updateFileBookLabels view

      -- Show core
      showCore view tag

    Nothing ->
      return ()

-- | Resets the labels of file pages so they show how many tags can be
-- found in the file in question
updateFileBookLabels :: SourceView -> IO ()
updateFileBookLabels SourceView {stateRef, sourceBook} = do
  SourceViewState {files,selection} <- readIORef stateRef
  forM_ files $ \f -> do
    let tagsInFile = case selection of
          Just tag -> case tagDebug tag of
            Just dbg -> filter ((== sourceFile f) . fileName) $ extSources dbg
            Nothing  -> []
          Nothing -> []
        title = takeFileName (sourceFile f) ++ suff
        suff | null tagsInFile = ""
             | otherwise       = " (" ++ show (length tagsInFile) ++ ")"
    putStrLn title
    page <- notebookGetNthPage sourceBook (sourcePage f)
    case page of
      Just p -> notebookGetTabLabel sourceBook p >>= \lbl -> case lbl of
        Just l  -> labelSetText (castToLabel l) title
        Nothing -> return ()
      Nothing -> print (sourcePage f)

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
        , any (\(IPRange l h) -> l <= ip && ip < h) (dbgRanges r)
                             -> Just r
      | otherwise            -> trace msg $ Nothing

  where msg = printf "Could not place IP %08x" ip

buildRangeMap :: [DebugEntry] -> IM.IntMap DebugEntry
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
  in samples

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
  let toTag freq (Just dbg)
        = tagFromDebug freq dbg
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

tagFromDebug :: Double -> DebugEntry -> Tag
tagFromDebug freq dbg@DebugEntry {..}
  = Tag { tagUnit = Just dbgUnit
        , tagName = Just $ zdecode dbgLabel
        , tagTick = dbgInstr
        , tagDebug = Just dbg
        , tagFreq = freq
        }

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
  tagListRef <- newIORef []
  tagTable <- textBufferGetTagTable sourceBuffer
  textTagTableForeach tagTable (\t -> modifyIORef tagListRef (t:))
  tagList <- readIORef tagListRef
  --mapM_ (textTagTableRemove tagTable) tagList
  return ()

{-  textTagTableForeach
  tagTable <- textTagTableNew
  set sourceBuffer [ textBufferTagTable := tagTable ]
  return ()-}

updateTextTags :: SourceView -> FileView -> IO ()
updateTextTags SourceView{..} FileView{..}= do

  -- Clear existing tags
  clearTextTags sourceBuffer

  -- Annotate source code (all tags, then specificially the selection)
  SourceViewState{tags,selection} <- readIORef stateRef
  let annotate = annotateTags sourceFile sourceView sourceBuffer
  annotate tags False
  case selection of
    Nothing  -> return ()
    Just sel -> annotate [sel] True

annotateTags :: String -> GtkSourceView.SourceView -> SourceBuffer -> [Tag] -> Bool -> IO ()
annotateTags sourceFile sourceView sourceBuffer tags sel = do
  tagTable <- textBufferGetTagTable sourceBuffer

  let freqMap2 = [(tagFreq, (1, src))
                 | Tag {tagFreq, tagDebug = Just dbg} <- tags
                 , src <- extSources dbg
                 , fileName src == sourceFile ]
      freqMap'  = nubSumBy (compare `F.on` snd) (\(f1, _) (f2, t) -> (f1+f2,t)) 
                  freqMap2

  -- "Safe" version of textBufferGetIterAtLineOffset
  lineCount <- textBufferGetLineCount sourceBuffer
  let textBufferGetIterAtLineOffsetSafe l c
        | l >= lineCount = textBufferGetEndIter sourceBuffer
        | otherwise = do iter <- textBufferGetIterAtLine sourceBuffer l
                         textIterForwardChars iter c
                         return iter

  forM_ freqMap' $ \(freq, (lvl, Span {..})) -> when (sel || lvl == 1) $ do

    -- Create color tag
    tag <- textTagNew Nothing
    let w = min 0xe000 $ max 0x0000 $ 0xffff - round (0x8000 * freq)
        --w' = 255 - round (fromIntegral 255 / fromIntegral (-lvl))
        clr | sel       = Color 0x8888 0x8888 0xffff
            | otherwise = Color w      w      w
    set tag [textTagBackgroundGdk := clr, textTagBackgroundSet := True]
    tagTable `textTagTableAdd` tag

      -- Place at code position
    start <- textBufferGetIterAtLineOffsetSafe (startLine-1) (startCol-1)
    end <- textBufferGetIterAtLineOffsetSafe (endLine-1) endCol
    textBufferApplyTag sourceBuffer tag start end

    --putStrLn $ "Annotating " ++ show (sl, sc, el, ec) ++ " with " ++ clr ++ "(lvl " ++ show lvl ++ ")"

  -- Scroll to largest tick
  let ticks = map (snd . snd) freqMap'
  when (sel && not (null ticks)) $ do
    let hpcSize (Span _ sl sc el ec) = (el-sl, ec-sc)
        largestTick = maximumBy (compare `F.on` hpcSize) ticks
        Span _ l c _ _ = largestTick
    iter <- textBufferGetIterAtLineOffset sourceBuffer (l-1) (c-1)
    _ <- textViewScrollToIter sourceView iter 0.2 Nothing
    return ()

-------------------------------------------------------------------------------

coreMarkCatFolded, coreMarkCatOpen :: String
coreMarkCatFolded = "threadscope-core-mark-folded"
coreMarkCatOpen = "threadscope-core-mark-open"

showCore :: SourceView -> Tag -> IO ()
showCore view@SourceView{coreBuffer,stateRef} tag = do

  -- Clear core buffer
  begin <- textBufferGetStartIter coreBuffer
  end <- textBufferGetEndIter coreBuffer
  sourceBufferRemoveSourceMarks coreBuffer begin end coreMarkCatFolded
  sourceBufferRemoveSourceMarks coreBuffer begin end coreMarkCatOpen
  textBufferSetText coreBuffer ""
  modifyIORef stateRef $ \s -> s { lineTags = [] }

  -- What to show?
  let (unit, coreText)
        | Just (_, core) <- findDbgElem dbgDCore $ tagDebug tag,
          Just unit <- tagUnit tag
        = (unit, prepareCore core)
        | Just n <- tagName tag,
          Just unit <- tagUnit tag,
          symTabPrefix `isPrefixOf` unit
        =  (unit,
              "--               *** Symbol from binary file ***\n\n" ++
              "-- No source code info could be found...\n" ++
              "-- This code was probably compiled without debugging annotations.\n\n" ++
              "-- Binary: " ++ drop (length symTabPrefix) unit ++ "\n" ++
              "-- Symbol: " ++ n)
        | otherwise
        = ("", "No data available.")

  -- Insert text
  iter <- textBufferGetEndIter coreBuffer
  showCorePlaceholders view iter unit coreText tag

-- | Writes core to source view, inserting placeholders where core
-- pieces have been left out.
showCorePlaceholders :: SourceView -> TextIter -> String -> String -> Tag -> IO ()
showCorePlaceholders SourceView{coreBuffer,stateRef} iter unit core tag = do

  -- Get start line
  startLine <- textIterGetLine iter

  -- Split code up
  let emitCore src = textBufferInsert coreBuffer iter src
      emitIcon name = sourceBufferCreateSourceMark coreBuffer
                      (Just $ show (unit, name) ) coreMarkCatFolded iter

  let prefix = "\"__Core__"
      go src []  = emitCore (reverse src)
      go src rest
        | prefix `isPrefixOf` rest
          = do let r' = drop (length prefix) rest
                   (name, r'') = break (== '\"') r'
               emitCore (reverse src)
               emitIcon name
               go "" (drop 1 r'')
      go src (r:rs)
          = go (r:src) rs
  go "" core

  -- Get end line, set tags
  endLine <- textIterGetLine iter
  modifyIORef stateRef $ \s@SourceViewState{lineTags} ->
    let (pr,su) = splitAt startLine lineTags
    in s { lineTags = pr ++ replicate (endLine - startLine) tag ++ su }

-- | Process core from its bytestring form into what we will actually
-- be showing (cleared and all)
prepareCore :: LBS.ByteString -> String
prepareCore = cleanupCore . map (chr . fromIntegral) . LBS.unpack

-- | Compresses core by merging lines with closing braces at the same
-- indention level.
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

-- | Called when a mark gets activated.
activateMark :: SourceView -> SourceBuffer -> TextIter -> IO ()
activateMark sview@SourceView{stateRef} coreBuffer pos = do

  -- Get our marks
  line <- textIterGetLine pos
  marks <- sourceBufferGetSourceMarksAtLine coreBuffer line coreMarkCatFolded

  -- Loop through marks. We really only expect zero or one of them per line.
  forM_ marks $ \mark -> do
    name <- textMarkGetName mark
    iter <- textBufferGetIterAtMark coreBuffer mark

    -- Look up core
    SourceViewState{coreMap,tags} <- readIORef stateRef
    case fmap reads name of
      Just [((unit, cname), "")]
        | Just entry <- Map.lookup (unit, cname) coreMap,
          Just (_, core) <- dbgDCore entry
        -> do -- Replace mark
              textBufferDeleteMark coreBuffer mark
              sourceBufferCreateSourceMark coreBuffer Nothing coreMarkCatOpen iter
              -- Get text to insert
              offset <- textIterGetLineOffset iter
              let tag = case find ((== Just entry) . tagDebug) tags of
                    Just t  -> t
                    Nothing -> tagFromDebug 0.0 entry
                  coreText = prepareCore core
                  indent = replicate offset ' '
                  indentedText = unlines $ map (indent++) $ lines coreText
              -- Insert text
              showCorePlaceholders sview iter unit (indentedText ++ indent) tag
              -- Save length to enable folding again?
      _ -> return ()

updateLineTags :: SourceView -> IO ()
updateLineTags SourceView {stateRef, coreView} = do

  -- Update tags
  modifyIORef stateRef $ \s ->
    let update t = case find (((==) `F.on` tagDebug) t) (tags s) of
          Just t' -> t'
          Nothing
            | Just d <- tagDebug t -> tagFromDebug 0.0 d
            | otherwise            -> t
     in s { lineTags = map update $ lineTags s }

  -- Redraw gutter
  gutter <- sourceViewGetGutter coreView TextWindowLeft
  sourceGutterQueueDraw gutter

-------------------------------------------------------------------------------

buildCoreMap :: [DebugEntry] -> CoreMap
buildCoreMap = Map.fromList . mapMaybe coreData
  where coreData entry@DebugEntry {dbgUnit, dbgDCore=Just (name, _)}
          = Just ((dbgUnit, name), entry)
        coreData _
          = Nothing

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
    go_proc name srcs ranges core (e:es) = case spec (ce_event e) of
      DebugSource { sline, scol, eline, ecol, file, name=name' }
        -> let span = Span file (fI sline) (fI scol) (fI eline) (fI ecol)
           in go_proc (name <|> Just name') (span:srcs) ranges core es
      DebugPtrRange { low, high }
        -> go_proc name srcs (IPRange (fromIntegral low) (fromIntegral high):ranges) core es
      DebugCore { coreBind, coreCode } | core == Nothing
        -> go_proc name srcs ranges (Just (coreBind, coreCode)) es
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

-- | Returns source code annotations to show for the given debug entry
extSources :: DebugEntry -> [Span]
extSources DebugEntry { dbgSources, dbgDName = Nothing, dbgParent = Just p}
  = dbgSources ++ extSources p
extSources DebugEntry { dbgSources}
  = dbgSources

------------------------------------------------------------------------------

-- | Subsume tags for the viewed list
subsumeTags :: [Tag] -> [Tag]
subsumeTags = nubSumBy cmp plus
   where cmp = compare `F.on` (dbgCmpId . tagDebug)
         dbgCmpId dbg = case findWithDbgElem dbgDCore dbg of
           Just DebugEntry { dbgDCore = Just (n, _) }
                       -> Left n
           _           -> Right (fmap dbgId dbg)
         t1 `plus` t2 = t1 { tagFreq = tagFreq t1 + tagFreq t2 }
