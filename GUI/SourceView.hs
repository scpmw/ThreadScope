
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

import Events.Core as Core

import GHC.RTS.Events

import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView hiding (SourceView, sourceViewNew)
import qualified Graphics.UI.Gtk.SourceView as GtkSourceView
import Graphics.Rendering.Cairo

import Data.Array
import Data.IORef
import Data.Maybe (mapMaybe, fromMaybe, isJust)
import Data.List
import Data.Word (Word32, Word64)
import qualified Data.Function as F
import qualified Data.IntMap as IM
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char (isSpace)
import qualified Data.ByteString as BS
import Data.Tree (Tree(Node))

import System.FilePath
import System.Directory (doesFileExist,getCurrentDirectory,canonicalizePath)

import Control.Monad (forM_, forM, when)
import Control.Applicative ((<$>))
import Control.Arrow (first, second)
import Control.DeepSeq (deepseq, NFData(..))
import Data.Monoid (mappend)

import Text.Printf

import Paths_threadscope (getDataFileName)

import Debug.Trace

-------------------------------------------------------------------------------

data FileView = FileView {
  sourceFile     :: FilePath,
  sourceLoad     :: IORef Bool,
  sourcePage     :: Int,
  sourceView     :: GtkSourceView.SourceView,
  sourceBuffer   :: SourceBuffer,
  sourceScroll   :: ScrolledWindow,
  sourceOverview :: DrawingArea
  }

data SourceView = SourceView {
  stateRef     :: IORef SourceViewState,
  sourceBook   :: Notebook,
  coreView     :: GtkSourceView.SourceView,
  coreBuffer   :: SourceBuffer,
  sourceFont   :: FontDescription,
  haskellLang  :: Maybe SourceLanguage,
  tagsTreeView :: TreeView,
  tagsStore    :: ListStore Tag,
  srcTagsTreeView :: TreeView,
  srcTagsStore :: TreeStore SourceTag,
  structRenderer :: CellRendererPixbuf,

  textHeight   :: !Int
  }

data SourceViewState
  = SourceViewState {
    eventsArr  :: EventsArray,
    dbgMap     :: DbgMap,
    rangeMap   :: RangeMap,
    coreMap    :: CoreMap,
    sourceMap  :: SourceMap,
    tags       :: [Tag],
    sourceTags :: [SourceTag],
    selection  :: Maybe Tag,
    srcSel     :: Maybe SourceTag,
    files      :: [FileView],
    searchDirs :: [FilePath],
    lineTags   :: [[Tag]],
    structBufMap :: StructPixbufMap
  }

data Span = Span {
  fileName    :: String,
  spanName    :: String,
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
  dbgUnit   :: String,
  dbgLabel  :: String,
  dbgRanges :: [IPRange],
  dbgDName  :: Maybe String,
  dbgInstr  :: Maybe Int,
  dbgParent :: Maybe DebugEntry,
  dbgSources:: [Span],
  dbgDCore  :: Maybe DebugEntryCore
  }

data DebugEntryCore = DebugEntryCore {
  dbgCoreBind :: String,
  dbgCoreCons :: String,
  dbgCoreCode :: CoreExpr -- lazy
  }

instance Eq DebugEntry where
  (==)    = (==) `F.on` dbgId
instance Ord DebugEntry where
  compare = compare `F.on` dbgId

type EventsArray = Array Int CapEvent
type RangeMap = IM.IntMap DebugEntry
type DbgMap = [DebugEntry]
type CoreMap = Map.Map (String, String, String) DebugEntry
type SourceMap = Map.Map Span [DebugEntry]

type StructPixbufMap = Map.Map [Tag] (Pixbuf, Pixbuf)

data Tag = Tag {
  -- | Compilation unit this tag belongs to
  tagUnit :: Maybe String,
  -- | Name of the tag.
  tagName :: Maybe String,
  -- | Instrumentation number of the tag.
  tagTick :: Maybe Int,
  -- | Debug data available for the tag
  tagDebug :: Maybe DebugEntry,
  -- | Approximate frequency of the tag getting hit
  tagFreq :: !Double,
  -- | Tag for the (top-level) entry node of subsumed tags
  tagEntry :: Tag
  }

data SourceTag = SourceTag {
  stagSource :: Span,
  stagTags :: [Tag],
  stagFreq :: !Double
  }

instance Eq Tag where
  (Tag m1 n1 t1 _ _ _) == (Tag m2 n2 t2 _ _ _)  = m1 == m2 && (n1 == n2 || t1 == t2)

instance Ord Tag where
  (Tag m1 n1 t1 _ _ _) `compare` (Tag m2 n2 t2 _ _ _) =
    (m1 `compare` m2) `mappend` (n1 `compare` n2) `mappend` (t1 `compare` t2)

instance NFData Tag where
  rnf Tag{..} = tagUnit `deepseq` tagName `deepseq` tagTick `deepseq` tagDebug `seq` ()

initViewState :: SourceViewState
initViewState = SourceViewState {
  eventsArr = listArray (0,0) [],
  dbgMap = [],
  rangeMap = IM.empty,
  coreMap = Map.empty,
  sourceMap = Map.empty,
  tags = [],
  sourceTags = [],
  selection = Nothing,
  srcSel = Nothing,
  files = [],
  searchDirs = [],
  lineTags = [],
  structBufMap = Map.empty
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

dumpDebug :: DebugEntry -> IO ()
dumpDebug DebugEntry{..} = do
  putStrLn $ dbgUnit ++ "/" ++ dbgLabel ++ ", " ++
    "IP ranges " ++ show dbgRanges ++ ", " ++
    (maybe "no dbg name" ("dbg name " ++) dbgDName) ++ ", " ++
    (maybe "no instr" (("instr " ++) . show) dbgInstr) ++ ", " ++
    (maybe "no parent" (\DebugEntry{..} -> "parent " ++ dbgUnit ++ "/" ++ dbgLabel) dbgParent) ++ ", " ++
    show (length dbgSources) ++ " source ranges, " {- ++
    (maybe "no core" (("core " ++) . show . dbgCoreCode) dbgDCore) -}

dumpTag :: Tag -> IO ()
dumpTag tag = do
  putStr $ printf "%02.2f" (100 * tagFreq tag) ++ "% "
  case tagDebug tag of
    Just dbg -> do
      let subs = getSubsumationEntry dbg
      if subs /= dbg
        then putStr $ " (part of " ++ dbgUnit subs ++ "/" ++ dbgLabel subs ++ "): "
        else putStr ": "
      dumpDebug dbg
    Nothing  -> putStrLn $ fromMaybe "no unit" (tagUnit tag) ++ "/" ++
                           fromMaybe "no name" (tagName tag) ++ ": no dbg..."

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
  sourceViewSetMarkCategoryIconFromStock coreView coreMarkCatOpenEnd (Just "") -- "Nothing" causes Gtk-Critical

  -- Find some layout parameters
  (wdt, textHeight) <- getTextDimensions coreView "100.0%"

  -- Set up cost annotations for core view
  costRenderer <- cellRendererTextNew
  set costRenderer [ cellTextSingleParagraphMode := True
                   , cellTextEditable := False
                   , cellTextAlignment := AlignRight
                   , cellWidth := wdt
                   ]
  coreGutter <- sourceViewGetGutter coreView TextWindowLeft
  sourceGutterInsert coreGutter costRenderer (-10)

  -- Set up cost structure annotations for core view
  structRenderer <- cellRendererPixbufNew
  sourceGutterInsert coreGutter structRenderer (-10)

  -- Create columns for tag list
  tagsTreeView <- getWidget castToTreeView "source_tagstree"
  tagsStore    <- listStoreNew []

  let mkColumn treeView render title = do
        col <- treeViewColumnNew
        treeViewColumnSetTitle col title
        treeViewColumnPackStart col render True
        treeViewAppendColumn treeView col
        return col

  tagFreqRender <- cellRendererTextNew
  tagFreqCol    <- mkColumn tagsTreeView tagFreqRender "%"
  tagModRender  <- cellRendererTextNew
  tagModCol     <- mkColumn tagsTreeView tagModRender "Module"
  tagDescRender <- cellRendererTextNew
  tagDescCol    <- mkColumn tagsTreeView tagDescRender "Name"
  tagCoreRender <- cellRendererTextNew
  tagCoreCol    <- mkColumn tagsTreeView tagCoreRender "Core"

  -- Set column content
  treeViewSetModel tagsTreeView tagsStore

  cellLayoutSetAttributes tagFreqCol tagFreqRender tagsStore $ \Tag{..} ->
    [ cellText := printf "%02.1f" (tagFreq * 100) ]

  cellLayoutSetAttributes tagCoreCol tagCoreRender tagsStore $ \Tag{..} ->
    [ cellText := fromMaybe "" (dbgCoreBind <$> findDbgElem dbgDCore tagDebug) ]
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

  -- Create columns for source tag tree
  srcTagsTreeView <- getWidget castToTreeView "source_tagstree1"
  srcTagsStore    <- treeStoreNew []

  srcTagFreqRender <- cellRendererProgressNew
  srcTagFreqCol    <- mkColumn srcTagsTreeView srcTagFreqRender "%"
  srcTagNameRender <- cellRendererTextNew
  srcTagNameCol    <- mkColumn srcTagsTreeView srcTagNameRender "Name"

  -- Set column content
  treeViewSetModel srcTagsTreeView srcTagsStore
  cellLayoutSetAttributes srcTagFreqCol srcTagFreqRender srcTagsStore $ \SourceTag{..} ->
    [ cellProgressText := Just $ printf "%02.1f" (stagFreq * 100)
    , cellProgressValue := round (stagFreq * 100) ]
  cellLayoutSetAttributes srcTagNameCol srcTagNameRender srcTagsStore $ \SourceTag{..} ->
    [ cellText := takeFileName (spanName stagSource) ]

  -- Set up search column
  treeViewSetSearchColumn srcTagsTreeView srcTagNameCol
  treeViewSetEnableSearch srcTagsTreeView True

  stateRef <- newIORef initViewState
  let srcView    = SourceView {..}

  -- Register events
  on tagsTreeView cursorChanged $
    updateTagSelection srcView
  on srcTagsTreeView cursorChanged $
    updateSrcTagSelection srcView
  on sourceBook switchPage $
    updateFileView srcView
  after coreView sourceViewLineMarkActivated $ \pos ->
    liftIO $ activateMark srcView coreBuffer pos

  sourceGutterSetCellDataFunc coreGutter costRenderer $ \_cell l _ -> do
    SourceViewState{lineTags} <- readIORef stateRef
    case splitAt l lineTags of
      (pre, (tag@Tag{..}:_):_) -> do
        clr <- lineTagClr srcView tag False
        -- only show text for first occurance
        let text | any (elem tag) pre = ""
                 | otherwise          = printf "%02.1f%%" (tagFreq * 100)
        set costRenderer [ cellText := text
                         , cellTextBackgroundColor := clr
                         , cellTextBackgroundSet := True ]
      _ -> set costRenderer [ cellText := ""
                            , cellTextBackgroundSet := False ]


  sourceGutterSetCellDataFunc coreGutter structRenderer $ \_cell l _ -> do
    SourceViewState{lineTags,structBufMap} <- readIORef stateRef
    case splitAt l lineTags of
      (pre,(ts:_)) | Just (pbuf, pbuf2) <- Map.lookup ts structBufMap ->
        set structRenderer [ cellPixbuf := case ts of
                                (t:_) | any (elem t) pre -> pbuf
                                _                        -> pbuf2 
                           ]
      _ -> return ()

  return srcView

-- | Returns the width of the given string in pixel when rendered on
-- screen. There is bound to be a better way of doing this, but I
-- haven't been able to find it so far. Reserved so I can come back to
-- this later.
getTextDimensions :: WidgetClass w => w -> String -> IO (Int, Int)
getTextDimensions widget str = do
  pango <- widgetGetPangoContext widget
  layout <- layoutText pango str
  (Rectangle _ _ wdt hgt, _) <- layoutGetPixelExtents layout
  return (wdt, hgt)

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
      -- mapM_ dumpDebug dbgMap

      -- Build range map
      let rangeMap = buildRangeMap dbgMap
      putStrLn $ printf "Range map has %d entries" (IM.size rangeMap)

      -- Build core map
      let coreMap = buildCoreMap dbgMap
      putStrLn $ printf "Core map has %d entries" (Map.size coreMap)
      -- mapM_ print $ Map.keys coreMap

      -- Build source map
      let sourceMap = buildSourceMap dbgMap
      putStrLn $ printf "Source map has %d entries" (Map.size sourceMap)

      -- Find initial tags
      let tags = tagsFromLocalTicks 0 eventsArr dbgMap ++
                 tagsFromLocalIPs2 0 eventsArr rangeMap
          selection = Nothing
          srcSel = Nothing

      let files = []
          lineTags = []
          sourceTags = []
          structBufMap = Map.empty
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
fileViewNew sview@SourceView {stateRef,haskellLang,sourceFont,sourceBook} sourceFile = do

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
  sourceScroll <- scrolledWindowNew Nothing Nothing
  containerAdd sourceScroll sourceView

  -- Create overview drawing area
  sourceOverview <- drawingAreaNew
  widgetSetSizeRequest sourceOverview 20 20

  -- Create HBox for placing scrolled window and overview next to each other
  hBox <- hBoxNew False 1
  boxPackStart hBox sourceScroll PackGrow 0
  boxPackEnd hBox sourceOverview PackNatural 0

  -- Create notebook page for the file
  sourcePage <- notebookAppendPage sourceBook hBox $ takeFileName sourceFile

  -- We don't actually load any content yet
  sourceLoad <- newIORef False

  -- Add to file view list
  let fileView = FileView {..}
  modifyIORef stateRef $ \state ->
    state { files = files state ++ [fileView] }

  -- Register for drawing overview
  on sourceOverview exposeEvent $ do
    region <- eventRegion
    liftIO (drawFileOverview sview fileView region)
    return True

  -- Show everything
  widgetShowAll hBox

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

drawFileOverview :: SourceView -> FileView -> Region -> IO ()
drawFileOverview
  SourceView{stateRef}
  FileView{sourceFile,sourceBuffer,sourceOverview,sourceScroll}
  exposeRegion = do

  -- Force full highlighting
  start <- textBufferGetStartIter sourceBuffer
  end <- textBufferGetEndIter sourceBuffer
  sourceBufferEnsureHighlight sourceBuffer start end

  -- Get line count
  lineCnt <- textBufferGetLineCount sourceBuffer

  -- Get drawing area size
  (wdt,hgt) <- widgetGetSize sourceOverview

  -- Get target coordinates
  (y0, y1) <- scrolledWindowGetVScrollbar sourceScroll >>= \m_bar -> case m_bar of
    Just bar -> do
      (barWdt, barHgt) <- widgetGetSize bar
      -- We want to derive the position of the "gray area" of the
      -- scroll bar, without the buttons. Here we simply assume that
      -- the bar width is the height of the buttons in order to remove
      -- them from the calculation. There is probably a better way to
      -- do this.
      p1 <- widgetTranslateCoordinates bar sourceOverview 0 barWdt
      p2 <- widgetTranslateCoordinates bar sourceOverview 0 (barHgt - barWdt)
      return (maybe 0 snd p1, maybe hgt snd p2)
    Nothing ->
      return (0, hgt)

  -- Get selection
  SourceViewState{tags} <- readIORef stateRef

  -- Get window, start rendering
  win <- widgetGetDrawWindow sourceOverview
  renderWithDrawable win $ do

    -- Set up clipping
    region exposeRegion
    clip

    -- Set up coordinate transformation
    let maxLineLen = 80
    translate 0 (fromIntegral y0)
    scale (fromIntegral wdt / (20 + maxLineLen)) (fromIntegral (y1-y0) / fromIntegral lineCnt)
    translate 20 0

    -- Start line drawing
    setLineWidth 1
    setSourceRGB 0.0 0.0 0.0
    setOperator OperatorSource

    -- Get first affected line
    Rectangle _ cy0 _ _ <- liftIO $ regionGetClipbox exposeRegion
    firstLine <- fmap (max 0 . snd) $ deviceToUser 0 (fromIntegral cy0)
    liftIO $ print firstLine

    itStart <- liftIO $ textBufferGetIterAtLine sourceBuffer (floor firstLine)
    itEnd <- liftIO $ textIterCopy itStart
    drawOverviewTags itStart itEnd

    -- Draw highlights: For everything, then for selection
    drawOverviewHighlight sourceFile tags False


drawOverviewTags :: TextIter -> TextIter -> Render ()
drawOverviewTags itStart itEnd = do

  -- Find next tag toggle
  found <- liftIO $ textIterForwardToTagToggle itEnd Nothing

  -- Get tags
  tagsOn <- liftIO $ textIterGetToggledTags itEnd True
  tagsOff <- liftIO $ textIterGetToggledTags itEnd False

  -- Collect interesting changes we need to do at this point
  actsOff <- forM tagsOff $ \tag -> do
    clrSet <- liftIO $ get tag textTagForegroundSet
    return $ if clrSet
             then [setSourceRGB 0.0 0.0 0.0]
             else []
  actsOn <- forM tagsOn $ \tag -> do
    clrSet <- liftIO $ get tag textTagForegroundSet
    if clrSet
      then do Color r g b <- liftIO $ get tag textTagForegroundGdk
              return [setSourceRGB (fromIntegral r / 65535)
                                   (fromIntegral g / 65535)
                                   (fromIntegral b / 65535)]
      else return []

  case concat actsOff ++ concat actsOn of

    -- At end?
    [] | not found -> do
      liftIO $ textIterForwardToEnd itEnd
      drawOverviewLines itEnd itStart

    -- Nothing to do? Continue
    [] -> drawOverviewTags itEnd itStart

    acts -> do

      -- Otherwise draw our lines
      drawOverviewLines itStart itEnd

      -- *Then* apply actions
      sequence_ acts

      -- Continue drawing
      iter <- liftIO $ textIterCopy itEnd
      drawOverviewTags iter itEnd

drawOverviewLines :: TextIter -> TextIter -> Render ()
drawOverviewLines itStart itEnd = do

  -- Get text slice
  src <- liftIO $ textIterGetSlice itStart itEnd

  -- Start pos
  startLine <- liftIO $ textIterGetLine itStart
  startCol <- liftIO $ textIterGetLineOffset itStart

  -- Iterate through lines
  forM_ (zip (lines src) [startLine..]) $ \(line, i) -> do

    -- Figure out line properites
    let ind = length (takeWhile isSpace line)
        col | i == startLine  = startCol
            | otherwise       = 0
        len = length line

    -- Draw
    moveTo (fromIntegral $ ind + col) (fromIntegral i)
    lineTo (fromIntegral $ len + col) (fromIntegral i)
    stroke
    return ()

{--
drawOverviewHighlight :: TextIter -> TextTag -> Render ()
drawOverviewHighlight iter tag = do

  -- Set color of tag
  Color r g b <- liftIO $  get tag textTagBackgroundGdk
  setSourceRGB (fromIntegral r / 65535 / 2)
               (fromIntegral g / 65535 / 2)
               (fromIntegral b / 65535 / 2)

  -- Save start line
  startLine <- liftIO $ textIterGetLine iter

  -- Find end of tag
  endIter <- liftIO $ textIterCopy iter
  liftIO $ textIterForwardToTagToggle endIter (Just tag)
  endLine <- liftIO $ textIterGetLine endIter

  -- Fill region with color
  {-liftIO $ putStrLn $ printf "%g %g"
    (fromIntegral startLine :: Double)
    (fromIntegral $ endLine - startLine + 1 :: Double)-}
  rectangle (-20)  (fromIntegral startLine)
              20   (fromIntegral $ endLine - startLine + 1)
  fill
--}

drawOverviewHighlight :: FilePath -> [Tag] -> Bool -> Render ()
drawOverviewHighlight file tags _select = do

  let spans = getFileSourceSpans file tags

  forM_ spans $ \(_, span) -> do
    rectangle (-20)  (fromIntegral $ startLine span)
                20   (fromIntegral $ endLine span - startLine span + 1)
    fill

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

  -- Build source tag list
  let getSources tag = map (mkSourceTag tag) $ findSources $ tagDebug tag
      -- If the debug entry has no source annotations at all, we
      -- assume that the source code annotations of the paraent
      -- context are most relevant (if any).
      findSources Nothing       = []
      findSources (Just DebugEntry{dbgSources, dbgParent})
        | not (null dbgSources) = dbgSources
        | otherwise             = findSources dbgParent
      mkSourceTag tag src = SourceTag { stagSource = src
                                      , stagTags = [tag]
                                      , stagFreq = tagFreq tag }
      sourceTags' = sortBy (compare `F.on` stagFreq) $
                    subsumeSrcTags False $ concatMap getSources tags'
      fileTags' = sortBy (compare `F.on` stagFreq) $
                  map mkFileSrcTag $
                  subsumeSrcTags True $ concatMap getSources tags'
      mkFileSrcTag s@SourceTag{stagSource=st@Span{fileName}}
        = s { stagSource = st { spanName = takeFileName fileName }}

  -- Update source tag selection
  let srcSel' = srcSel >>= (\s -> find (sourceTagEquiv s) sourceTags')

  forM_ sourceTags' $ \SourceTag{..} -> do
    putStrLn $ printf "** %02.2f %s:%s" (100 * stagFreq) (fileName stagSource) (spanName stagSource)
    mapM_ dumpTag stagTags

  -- Set new state
  tags' `deepseq`
    writeIORef stateRef state{ tags=tags', sourceTags=sourceTags', selection=selection', srcSel=srcSel' }
  putStrLn " * Current tags:"
  mapM_ dumpTag tags'

  -- Update views
  postGUISync $ do
    updateTagsView tagsStore tags'
    updateSourceTagsView srcTagsStore sourceTags' fileTags'
    updateLineTags view
    setTagSelection view selection'
    setSrcTagSelection view srcSel'

updateTagsView :: ListStore Tag -> [Tag] -> IO ()
updateTagsView tagsStore tags = do
  let subsumed = subsumeTags tags
      sorted = sortBy (flip (compare `F.on` tagFreq)) subsumed
  listStoreClear tagsStore
  mapM_ (listStoreAppend tagsStore) sorted

updateSourceTagsView :: TreeStore SourceTag -> [SourceTag] -> [SourceTag] -> IO ()
updateSourceTagsView tagsStore tags fileTags = do

  -- Construct tree
  let unitTree x = Node x []
      stagForest file
        = map unitTree $ reverse $ filter ((== file) . fileName . stagSource) tags
  treeStoreClear tagsStore
  treeStoreInsertForest tagsStore [] 0 $ reverse
    [ Node st (stagForest $ fileName $ stagSource st) | st <- fileTags ]

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
      clearCore view
      showCore view (tagEntry tag)

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
    page <- notebookGetNthPage sourceBook (sourcePage f)
    case page of
      Just p -> notebookGetTabLabel sourceBook p >>= \lbl -> case lbl of
        Just l  -> labelSetText (castToLabel l) title
        Nothing -> return ()
      Nothing -> print (sourcePage f)

------------------------------------------------------------------------------

updateSrcTagSelection :: SourceView -> IO ()
updateSrcTagSelection view@SourceView{..} = do
  state@SourceViewState{..} <- readIORef stateRef

  -- Read selection from list view, write to state
  tagSelect <- treeViewGetSelection srcTagsTreeView
  m_iter <- treeSelectionGetSelected tagSelect
  select' <- case m_iter of
    Just iter -> do path <- treeModelGetPath srcTagsStore iter
                    tag <- treeStoreGetValue srcTagsStore path
                    return (Just tag)
    Nothing   -> return Nothing
  writeIORef stateRef state {
    srcSel = select'
    }

  case select' of
    Just stag -> do

      -- Find file to show
      let unit = fileName $ stagSource stag
      fileView <- fileViewGet view unit
      notebookSetCurrentPage sourceBook (sourcePage fileView)

      -- Scroll to function

      -- Note: We are creating a mark here, as scrolling to an
      -- iterator position is not reiable with freshly opened text
      -- buffers.
      let span = stagSource stag
          buf = sourceBuffer fileView
      textIter <- textBufferGetIterAtLineOffsetSafe buf
                    (startLine span) (startCol span)
      let markName = "threadscope-scroll-mark"
      m_scrollMark <- textBufferGetMark buf markName
      scrollMark <- case m_scrollMark of
        Just mark -> do textBufferMoveMark buf mark textIter
                        return mark
        Nothing   -> textBufferCreateMark buf (Just markName) textIter True
      textViewScrollToMark (sourceView fileView) scrollMark 0 (Just (0.5,0.5))

      -- Update labels
      updateFileBookLabels view

      -- Subsume tags
      let tags = subsumeTags $ map tagEntry $ stagTags stag

      -- Show cores
      clearCore view
      forM_ (sortBy (compare `F.on` tagFreq) tags) $ \tag -> do
        iter <- textBufferGetEndIter coreBuffer
        writeSource view iter [] $ "---- " ++ fromMaybe "???" (tagName tag) ++ " ----\n"
        showCore view (tagEntry tag)
        iter <- textBufferGetEndIter coreBuffer
        writeSource view iter [] "\n"

    Nothing ->
      return ()


setSrcTagSelection :: SourceView -> Maybe SourceTag -> IO ()
setSrcTagSelection SourceView{..} _ = do
  tagSelect <- treeViewGetSelection srcTagsTreeView
  treeSelectionUnselectAll tagSelect
setSrcTagSelection SourceView{..} (Just stag) = do
  tagSelect <- treeViewGetSelection srcTagsTreeView
  treeSelectionUnselectAll tagSelect
  treeModelForeach srcTagsStore $ \i -> do
    p <- treeModelGetPath srcTagsStore i
    t <- treeStoreGetValue srcTagsStore p
    if stag `sourceTagEquiv` t
      then treeSelectionSelectPath tagSelect p >> return True
      else return False

sourceTagEquiv :: SourceTag -> SourceTag -> Bool
sourceTagEquiv SourceTag{stagSource=s1} SourceTag{stagSource=s2}
  = fileName s1 == fileName s2 && spanName s1 == spanName s2


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
              tag = Tag { tagUnit   = dbgUnit <$> dbg
                        , tagName   = Nothing
                        , tagTick   = Just (fromIntegral tick)
                        , tagDebug  = dbg
                        , tagFreq   = freq
                        , tagEntry  = tag
                        }
          in tag

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
_ipSampleWinSize, ipSampleStdDev :: Timestamp
_ipSampleWinSize = 50 * 1000 * 1000
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
        = let tag = Tag { tagUnit = Nothing
                        , tagName = Just "(unrecognized)"
                        , tagTick = Nothing
                        , tagDebug = Nothing
                        , tagFreq = freq
                        , tagEntry = tag
                        }
          in tag

      weighted = findLocalIPsWeighted startIx eventsArr rangeMap
      grandSum = sum $ map fst weighted
      tags = map (uncurry toTag . first (/grandSum)) weighted

      ord = compare `F.on` (fmap dbgId . tagDebug)
      t1 `plus` t2 = t1 { tagFreq = tagFreq t1 + tagFreq t2 }
  in nubSumBy ord plus tags

tagFromDebug :: Double -> DebugEntry -> Tag
tagFromDebug freq dbg@DebugEntry {..}
  = let tag = Tag { tagUnit = Just dbgUnit
                  , tagName = Just $ zdecode dbgLabel
                  , tagTick = dbgInstr
                  , tagDebug = Just dbg
                  , tagFreq = freq
                  , tagEntry = tag
                  }
    in tag

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
  --tagList <- readIORef tagListRef
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

-- | From a list of tags, gives the source ranges that are covered by
-- the tags in the given file.
getFileSourceSpans :: String -> [Tag] -> [(Double, Span)]
getFileSourceSpans sourceFile tags =
  let spans = [(tagFreq, src)
              | Tag {tagFreq, tagDebug = Just dbg} <- tags
              , src <- extSources dbg
              , fileName src == sourceFile ]
   in nubSumBy (compare `F.on` snd) (\(f1, _) (f2, t) -> (f1+f2,t)) spans


  -- "Safe" version of textBufferGetIterAtLineOffset
textBufferGetIterAtLineOffsetSafe :: TextBufferClass self => self -> Int -> Int -> IO TextIter
textBufferGetIterAtLineOffsetSafe buf l c = do
  lineCount <- textBufferGetLineCount buf
  if l >= lineCount
    then textBufferGetEndIter buf
    else do iter <- textBufferGetIterAtLine buf l
            textIterForwardChars iter c
            return iter

annotateTags :: String -> GtkSourceView.SourceView -> SourceBuffer -> [Tag] -> Bool -> IO ()
annotateTags sourceFile sourceView sourceBuffer tags sel = do
  tagTable <- textBufferGetTagTable sourceBuffer

  -- Get spans to annotate
  let freqMap' = getFileSourceSpans sourceFile tags
  forM_ freqMap' $ \(freq, Span {..}) -> do

    -- Create color tag
    tag <- textTagNew Nothing
    let w = min 0xe000 $ max 0x0000 $ 0xffff - round (0x8000 * freq)
        --w' = 255 - round (fromIntegral 255 / fromIntegral (-lvl))
        clr | sel       = Color 0x8888 0x8888 0xffff
            | otherwise = Color w      w      w
    set tag [textTagBackgroundGdk := clr, textTagBackgroundSet := True]
    tagTable `textTagTableAdd` tag

      -- Place at code position
    start <- textBufferGetIterAtLineOffsetSafe sourceBuffer (startLine-1) (startCol-1)
    end <- textBufferGetIterAtLineOffsetSafe sourceBuffer (endLine-1) endCol
    textBufferApplyTag sourceBuffer tag start end

    --putStrLn $ "Annotating " ++ show (sl, sc, el, ec) ++ " with " ++ clr ++ "(lvl " ++ show lvl ++ ")"

  -- Scroll to largest tick
  let ticks = map snd freqMap'
  when (sel && not (null ticks)) $ do
    let hpcSize (Span _ _ sl sc el ec) = (el-sl, ec-sc)
        largestTick = maximumBy (compare `F.on` hpcSize) ticks
        (Span _ _ l c _ _) = largestTick
    iter <- textBufferGetIterAtLineOffset sourceBuffer (l-1) (c-1)
    postGUIAsync $ do textViewScrollToIter sourceView iter 0.2 Nothing
                      return ()
    return ()

-------------------------------------------------------------------------------

coreMarkCatFolded, coreMarkCatOpen, coreMarkCatOpenEnd :: String
coreMarkCatFolded = "threadscope-core-mark-folded"
coreMarkCatOpen = "threadscope-core-mark-open"
coreMarkCatOpenEnd = "threadscope-core-mark-open-end"

-- | Clear core buffer
clearCore :: SourceView -> IO ()
clearCore SourceView{coreBuffer,stateRef} = do

  begin <- textBufferGetStartIter coreBuffer
  end <- textBufferGetEndIter coreBuffer
  deleteCore coreBuffer begin end
  modifyIORef stateRef (\state -> state { lineTags = [] } )

-- | Deletes core between two given iterators, making sure to remove
-- all present tags as well
deleteCore :: SourceBuffer -> TextIter -> TextIter -> IO ()
deleteCore coreBuffer begin end = do
  sourceBufferRemoveSourceMarks coreBuffer begin end coreMarkCatFolded
  sourceBufferRemoveSourceMarks coreBuffer begin end coreMarkCatOpen
  sourceBufferRemoveSourceMarks coreBuffer begin end coreMarkCatOpenEnd
  textBufferDelete coreBuffer begin end

showCore :: SourceView -> Tag -> IO ()
showCore sview@SourceView{coreBuffer,stateRef} tag = do

  -- Collect information about stuff to show
  SourceViewState{tags} <- readIORef stateRef
  let dbg = findWithDbgElem dbgDCore $ tagDebug tag
      ltags = case dbg of
        Just d  -> [lookupTagByDebug tags d]
        Nothing -> []

  -- Write data to buffer
  iter <- textBufferGetEndIter coreBuffer
  case tagUnit tag of
    Just unit | Just core <- fmap dbgCoreCode (dbg >>= dbgDCore)
      -> printCore sview iter unit ltags core
    Just unit | symTabPrefix `isPrefixOf` unit, Just n <- tagName tag
      -> writeSource sview iter ltags $ concat
            [ "--               *** Symbol from binary file ***\n\n"
            , "-- No source code info could be found...\n"
            , "-- This code was probably compiled without debugging annotations.\n\n"
            , "-- Binary: ", drop (length symTabPrefix) unit, "\n"
            , "-- Symbol: ", n ]
    _ -> writeSource sview iter ltags $ "No data available."

  -- Generate structure pixbufs
  updateStructPixbufMap sview 

-- | Writes a string of source code into the core view, registering
-- the debug-entry to associate with it.
writeSource :: SourceView -> TextIter -> [Tag] -> String -> IO ()
writeSource SourceView{stateRef,coreBuffer} iter ltags src = do

  -- Get iterator line, number of lines to insert
  line <- textIterGetLine iter
  let lines = length $ filter (=='\n') src

  -- Insert tags
  when (lines > 0) $ modifyIORef stateRef $ \s@SourceViewState{lineTags} ->
    let (pr,su) = splitAt (line + 1) lineTags
        su'     = case su of [] -> [ltags]; other -> other
        pr'     = pr ++ replicate (line + 1 - length pr) ltags
    in s { lineTags = pr' ++ replicate lines ltags ++ su' }

  -- Insert text
  textBufferInsert coreBuffer iter src

-- | Names used for markers in core buffer
markNameStart,markNameEnd :: String -> (String, String) -> String
markNameStart unit (bind, cons) = show (unit, bind, cons)
markNameEnd unit ref = markNameStart unit ref ++ "_end"

-- | Creates an closed core placeholder at the given position
writeFoldedMark :: SourceView -> TextIter -> String -> (String, String) -> IO ()
writeFoldedMark SourceView{coreBuffer} iter unit ref =
  sourceBufferCreateSourceMark coreBuffer
    (Just $ markNameStart unit ref) coreMarkCatFolded iter >> return ()

-- | Creates an open core placeholder at the given position
writeOpenMarkStart :: SourceView -> TextIter -> String -> (String, String) -> IO ()
writeOpenMarkStart SourceView{coreBuffer} iter unit ref = do
  sourceBufferCreateSourceMark coreBuffer
    (Just $ markNameStart unit ref) coreMarkCatOpen iter >> return ()

-- | Creates an open core placeholder at the given position
writeOpenMarkEnd :: SourceView -> TextIter -> String -> (String, String) -> IO ()
writeOpenMarkEnd SourceView{coreBuffer} iter unit ref  =
  sourceBufferCreateSourceMark coreBuffer
    (Just $ markNameEnd unit ref) coreMarkCatOpenEnd iter >> return ()

-- | Looks up a tag for the given debug entry - or constructs a new
-- one with zero cost attributed to it.
lookupTagByDebug :: [Tag] -> DebugEntry -> Tag
lookupTagByDebug tags dbg = case find ((== Just dbg) . tagDebug) tags of
  Just t  -> t
  Nothing -> tagFromDebug 0 dbg

-- | Sets the tag for the given line
writeLineTag :: SourceView -> TextIter -> [Tag] -> IO ()
writeLineTag SourceView{stateRef} iter ltags = do
  line <- textIterGetLine iter
  state@SourceViewState{lineTags} <- readIORef stateRef
  let (pr, su) = splitAt line lineTags
  writeIORef stateRef $ state { lineTags = pr ++ (ltags : drop 1 su) }

-- | Callback for Core pretty-printer
corePrinter :: SourceView -> TextIter -> String -> [Tag]
               -> Either String (String, String) -> IO () -> IO ()
corePrinter sview iter _    ltags (Left s) cont =
  writeSource sview iter ltags s >> cont
corePrinter sview iter unit ltags (Right (bind, cons)) cont = do

  -- Lookup core piece
  let SourceView{stateRef} = sview
  SourceViewState{coreMap,tags} <- readIORef stateRef
  let dbg = case ltags of
        []  -> Nothing
        t:_ -> tagDebug t
  case Map.lookup (unit, bind, cons) coreMap of
    Nothing
      -> writeSource sview iter tags "#ref!#"

    -- Subsumed core piece? Generate open fold
    Just cdbg | Just core <- dbgDCore cdbg,
                Just (getSubsumationEntry cdbg) == fmap getSubsumationEntry dbg
      -> do let ltags' = lookupTagByDebug tags cdbg : ltags
            writeOpenMarkStart sview iter unit (bind, cons)
            printCore sview iter unit ltags' (dbgCoreCode core)
            writeOpenMarkEnd sview iter unit (bind, cons)

    -- Otherwise generate closed fold
    Just cdbg
      -> do let ltags' = lookupTagByDebug tags cdbg : ltags
            writeLineTag sview iter ltags'
            writeFoldedMark sview iter unit (bind, cons)

  -- Continue printing
  cont

-- | Writes core to source view, including folded and open markers
-- where appropriate.
printCore :: SourceView -> TextIter -> String -> [Tag] -> CoreExpr -> IO ()
printCore sview@SourceView{coreBuffer} iter unit ltags core = do

  -- Make sure start line carries the right tag
  writeLineTag sview iter ltags

  -- Find indention level, and clear it. Note we assume here that
  -- there isn't anything on the line!
  lineOff <- textIterGetLineOffset iter
  when (lineOff > 0) $ do
    iter2 <- textIterCopy iter
    textIterBackwardChars iter lineOff
    textBufferDelete coreBuffer iter iter2

  -- Start rendering
  renderExpr (lineOff + 100) lineOff
    (corePrinter sview iter unit ltags)
    (return ())
    core

{--

-- | Writes core to source view, inserting placeholders where core
-- pieces have been left out.
showCorePlaceholders :: SourceView -> TextIter -> String -> Maybe DebugEntry -> String -> IO ()
showCorePlaceholders sview@SourceView{coreBuffer,stateRef} iter unit dbg core = do

  -- Find a good indention level
  lineOff <- textIterGetLineOffset iter
  iter2 <- textIterCopy iter
  textIterBackwardChars iter2 lineOff
  textIterForwardWordEnd iter2
  textIterBackwardWordStart iter2

  -- Get our indention level
  indent <- (+2) <$> textIterGetLineOffset iter2
  let indentStr = replicate indent ' '

  -- How to write folded code. Emit open or closed depending on
  -- whether we subsume it by default.
  --
  -- Note: This is getting even stranger by the minute. Needs a rewrite.
  SourceViewState{coreMap} <- readIORef stateRef
  let emitFold bind cons
        | Just entries <- Map.lookup (unit, bind, cons) coreMap,
          Just d <- fmap getSubsumationEntry dbg,
          all ((== d) . getSubsumationEntry) entries
        = forM_ entries $ \e -> case dbgDCore e of
            Just core -> do
              sourceBufferCreateSourceMark coreBuffer
                (Just $ markName bind cons) coreMarkCatOpen iter
              showCorePlaceholders sview iter unit (Just e)
                (prepareCore cons $ dbgCoreCode core)
              sourceBufferCreateSourceMark coreBuffer
                (Just $ markNameEnd bind cons) coreMarkCatOpenEnd iter
              return ()
            Nothing ->
              return ()
        | otherwise
        = do sourceBufferCreateSourceMark coreBuffer
               (Just $ markName bind cons) coreMarkCatFolded iter
             return ()
      markName bind cons = show (unit, bind, cons)
      markNameEnd bind cons = markName bind cons ++ "_end"

  -- Split up source code
  let corePrefix = "\"__Core__"
      defAlt = "__DEFAULT ->"
      go src _    []
        = writeCore sview iter dbg (reverse src)
      go src csrc ('\n':rs)
        = go (indentStr++'\n':src) csrc rs

      -- Finding "__DEFAULT ->" causes us to assume that we are in a
      -- case, which might have an Core link in it. As the actual link
      -- can come after arbitrary whitespace (or not at all), we just
      -- save back the source at this position so we can later decide
      -- what to do.
      go src _    rest
        | defAlt `isPrefixOf` rest
        = go (reverse defAlt ++ src) (Just src) (drop (length defAlt) rest)

      go src csrc rest
        | corePrefix `isPrefixOf` rest
          = do let r' = drop (length corePrefix) rest
                   (name, r'') = break (== '\"') r'
               writeCore sview iter dbg $ case csrc of
                 Just s  -> reverse s
                 Nothing -> reverse src
               emitFold name "" -- FIXME
               go "" Nothing (dropWhile isSpace $ drop 1 r'')

      -- Stop passing through "case" source pointe on non-whitespace
      go src csrc (r:rs)
          = go (r:src) (if isSpace r then csrc else Nothing) rs

  go "" Nothing core

-- | Process core from its bytestring form into what we will actually
-- be showing (cleared and all)
prepareCore :: String -> CoreExpr -> String
prepareCore _cons = show --cleanupCore calt . map (chr . fromIntegral) . LBS.unpack

-- | Compresses core by merging lines with closing braces at the same
-- indention level.
cleanupCore :: Bool -> String -> String
cleanupCore calt
  | calt      = unlines . go . ("":) . lines
  | otherwise = unlines . go . lines
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

--}

-- | Called when a mark gets activated.
activateMark :: SourceView -> SourceBuffer -> TextIter -> IO ()
activateMark sview@SourceView{stateRef} coreBuffer pos = do

  return ()
  
  {--
  -- Get our marks
  line <- textIterGetLine pos
  foldedMarks <- sourceBufferGetSourceMarksAtLine coreBuffer line coreMarkCatFolded
  openMarks <- sourceBufferGetSourceMarksAtLine coreBuffer line coreMarkCatOpen

  -- Loop through folded marks. We really only expect zero or one of them per line.
  forM_ foldedMarks $ \mark -> do
    name <- textMarkGetName mark
    iter <- textBufferGetIterAtMark coreBuffer mark

    -- Look up core
    SourceViewState{coreMap} <- readIORef stateRef
    case fmap (break (== '(')) name of
      Just (mtype, mname)
        | [((unit, cname, cons), "")] <- reads mname
        , Just entries <- Map.lookup (unit, cname, cons) coreMap
        -> forM_ entries $ \entry -> do
          -- Replace mark
          let core = dbgCoreCode $ fromJust $ dbgDCore entry
          textBufferDeleteMark coreBuffer mark
          sourceBufferCreateSourceMark coreBuffer name coreMarkCatOpen iter
          -- Get text to insert
          let coreText = prepareCore cons core
          -- Insert text
          showCorePlaceholders sview iter unit (Just entry) coreText
          sourceBufferCreateSourceMark coreBuffer (fmap (++"_end") name) coreMarkCatOpenEnd iter
          return ()
      _ -> return ()

  -- Loop through open marks. Same as above.
  forM_ openMarks $ \mark -> do
    name <- textMarkGetName mark
    iter <- textBufferGetIterAtMark coreBuffer mark

    -- Find end mark
    m_markEnd <- case name of
      Just n  -> textBufferGetMark coreBuffer (n++"_end")
      Nothing -> return Nothing
    case m_markEnd of
      Nothing -> return ()
      Just markEnd -> do
        -- Get iterator for end
        iterEnd <- textBufferGetIterAtMark coreBuffer markEnd
        -- Find out how many lines we span
        lineEnd <- textIterGetLine iterEnd
        -- Remove text
        clearCore coreBuffer iter iterEnd
        -- Insert closed mark
        sourceBufferCreateSourceMark coreBuffer name coreMarkCatFolded iter
        -- Remove line annotations
        modifyIORef stateRef $ \s ->
          s { lineTags = let (pr, re) = splitAt line (lineTags s)
                         in pr ++ drop (lineEnd - line) re
            }
--}
updateLineTags :: SourceView -> IO ()
updateLineTags SourceView {stateRef, coreView} = do

  -- Update tags
  modifyIORef stateRef $ \s ->
    let update t = case find (((==) `F.on` tagDebug) t) (tags s) of
          Just t' -> t'
          Nothing
            | Just d <- tagDebug t -> tagFromDebug 0.0 d
            | otherwise            -> t
     in s { lineTags = map (map update) $ lineTags s }

  -- Redraw gutter
  gutter <- sourceViewGetGutter coreView TextWindowLeft
  sourceGutterQueueDraw gutter

-------------------------------------------------------------------------------

buildCoreMap :: [DebugEntry] -> CoreMap
buildCoreMap = Map.fromList . mapMaybe coreData
  where coreData entry@DebugEntry {dbgUnit, dbgDCore=Just core}
          = Just ((dbgUnit, dbgCoreBind core, dbgCoreCons core), entry)
        coreData _
          = Nothing

-------------------------------------------------------------------------------

buildSourceMap :: [DebugEntry] -> SourceMap
buildSourceMap = Map.fromListWith (++) . concatMap mkEntry
  where mkEntry entry@DebugEntry{dbgSources} =
          map (\s -> (s, [entry])) dbgSources

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
        -> let (srcs, ranges, core) = go_proc [] [] Nothing es
               p_entry = parent >>= flip IM.lookup iMapI . fI
               name = case find ((== mfile) .fileName) srcs of
                 Just span              -> Just $ spanName span
                 Nothing | null srcs    -> Nothing
                         | otherwise    -> Just $ spanName $ head srcs
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

    go_proc srcs ranges core [] = (reverse srcs, ranges, core)
    go_proc srcs ranges core (e:es) = case spec (ce_event e) of
      DebugSource { sline, scol, eline, ecol, file, name=name' }
        -> let span = Span file name' (fI sline) (fI scol) (fI eline) (fI ecol)
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

------------------------------------------------------------------------------

-- | Subsume tags for the viewed list
subsumeTags :: [Tag] -> [Tag]
subsumeTags = nubSumBy cmp plus
   where cmp = compare `F.on` (fmap dbgCmpId . tagDebug)
         dbgCmpId dbg = dbgId $ getSubsumationEntry dbg
         t1 `plus` t2 = let ss = fmap getSubsumationEntry $ tagDebug t1 in
           t1 {
             tagEntry = case () of
                _ | tagDebug t1 == ss  -> t1
                  | tagDebug t2 == ss  -> t2
                  | otherwise          -> t2,
             tagDebug = ss,
             tagFreq = tagFreq t1 + tagFreq t2
           }

-- | Finds debug entry
getSubsumationEntry :: DebugEntry -> DebugEntry
getSubsumationEntry entry = case dbgParent entry of
  Nothing -> entry
  Just parent -> case dbgDCore entry of
    Just (DebugEntryCore _ _ Lam{}) -> entry
    _                               -> getSubsumationEntry parent

------------------------------------------------------------------------------

{- --- Not used right now... But might come in useful in future

-- | Returns whether a span "contains" another span - or isn't comparable
containsSpan :: Span -> Span -> Maybe Ordering
containsSpan s1 s2
  | spanName s1 /= spanName s2  = Nothing
  | fileName s1 == fileName s2  = Nothing
  | otherwise = case (startCmp, endCmp) of
    (EQ, EQ) -> Just EQ
    (LT, EQ) -> Just GT
    (LT, GT) -> Just GT
    (EQ, GT) -> Just GT
    (GT, EQ) -> Just LT
    (GT, LT) -> Just LT
    (EQ, LT) -> Just LT
    _        -> Nothing
  where startCmp = (startLine s1, startCol s1) `compare` (startLine s2, startCol s2)
        endCmp = (endLine s1, endCol s1) `compare` (endLine s2, endCol s2)
-}

-- | Subsume source tags for the viewed list
--
-- FIXME: Note that this will merge distinct source ranges, which
-- might lead to the generated source ranges to cover more source than
-- the original spans.
subsumeSrcTags :: Bool -> [SourceTag] -> [SourceTag]
subsumeSrcTags filesOnly = nubSumBy (cmpSpan `F.on` stagSource) plus
  where cmpSpan s1 s2
          | filesOnly = fileName s1 `compare` fileName s2
          | otherwise = (spanName s1, fileName s1) `compare` (spanName s2, fileName s2)
        plusSpan s1 s2 =
          let (sl, sc) = min (startLine s1, startCol s1) (startLine s2, startCol s2)
              (el, ec) = min (endLine s1, endCol s1) (endLine s2, endCol s2)
          in s1 { startLine = sl, startCol = sc, endLine = el, endCol = ec }
        plus st1 st2 = let tags' = nub (stagTags st1 ++ stagTags st2) in
          st1 { stagSource = plusSpan (stagSource st1) (stagSource st2)
              , stagTags = tags'
              , stagFreq = sum $ map tagFreq tags' }


------------------------------------------------------------------------------

clearStructPixbufMap :: SourceView -> IO ()
clearStructPixbufMap sview@SourceView{stateRef} =
  modifyIORef stateRef $ \s -> s { structBufMap = Map.empty }

-- | Updates the map so that there is an entry (exactly) for all
-- elements of the list.
updateStructPixbufMap :: SourceView -> IO ()
updateStructPixbufMap sview@SourceView{textHeight,coreBuffer,coreView,stateRef,structRenderer} = do

  -- Updating doesn't make too much sense... Just clear
  -- everytime. Rest of code needs to be cleaned up...
  clearStructPixbufMap sview

  -- Get current line tags and pix buf map
  SourceViewState{lineTags,structBufMap} <- readIORef stateRef

  -- Build set of tags, filter out what we don't need
  let tagset  = Map.keysSet structBufMap
      tagset' = Set.fromList lineTags

  -- Get line height
  iter <- textBufferGetStartIter coreBuffer
  (_, lineHeight) <- textViewGetLineYrange coreView iter

  -- Determine width
  let tcnt = maximum $ 1 : map length lineTags
      bufWdt = tcnt * 2
  set structRenderer [ cellWidth := bufWdt ]

  -- Helper for filling a rectangle with a color. Note that "Color" is
  -- range 0..65535, while pixBufFill wants 0..255. Strange stuff.
  let fillRect pbuf x y wdt hgt (Color r g b) = do
        subbuf <- pixbufNewSubpixbuf pbuf x y wdt hgt
        let cconv x = fromIntegral (x `div` 256)
        pixbufFill subbuf (cconv r) (cconv g) (cconv b) 255

  -- Get background color
  style <- widgetGetStyle coreView
  bgColor <- styleGetBackground style StateNormal

  -- create new bufs
  let bufHgt = lineHeight
      toGen = Set.toList $ tagset' `Set.difference` tagset
      tcnt = maximum $ 1 : map length lineTags
  newBufs <- forM toGen $ \ts -> do
    -- Create new pix buffer, fill it with a color
    -- The color will be overwritten below, except when ts == []
    pbuf <- pixbufNew ColorspaceRgb False 8 bufWdt bufHgt
    fillRect pbuf 0 0 bufWdt bufHgt bgColor
    -- Fill a rect for each nested tag
    forM_ (zip [1..] ts) $ \(n,t) -> do
      -- Create sub-pixbuf of the area we want to fill
      let depth            = length ts - n
          wdt | depth == length ts - 1
                           = bufWdt - 2 * depth - 1
              | otherwise  = 1
      -- Fill with tag foreground color
      fillRect pbuf (2*depth) 0 1 bufHgt =<< lineTagClr sview t True
      fillRect pbuf (2*depth+1) 0 wdt bufHgt =<< lineTagClr sview t False
    -- Clone pix buf for start marker
    pbuf2 <- pixbufCopy pbuf
    when (length ts > 0) $ do
      let depth = length ts - 1
          wdt   = bufWdt - 2 * depth
      fillRect pbuf2 (2*depth) 0 wdt 1 =<< lineTagClr sview (head ts) True
    return (ts, (pbuf, pbuf2))

  -- Create & set new map
  let newMap1 = Map.fromList newBufs
      newMap2 = Map.filterWithKey (\k _ -> k `Set.member` tagset') structBufMap
  modifyIORef stateRef (\s -> s { structBufMap = newMap1 `Map.union` newMap2 } )

-- | Gives color to assign to a line tag
lineTagClr :: SourceView -> Tag -> Bool -> IO Color
lineTagClr SourceView{stateRef,coreView} tag bg = do
  style <- widgetGetStyle coreView
  SourceViewState{srcSel} <- readIORef stateRef
  let state = case srcSel of
        Just SourceTag{stagTags}
          | tag `elem` stagTags -> StateSelected
        _                       -> StateNormal
  lColor <- styleGetDark style state
  bgColor <- styleGetBackground style state
  let m | bg        = 1.1 :: Double
        | otherwise = max 0 (1 + log (tagFreq tag) / 7)
      mix m (Color r1 g1 b1) (Color r2 g2 b2)
        = Color (round $ m * fromIntegral r1 + (1-m) * fromIntegral r2)
                (round $ m * fromIntegral g1 + (1-m) * fromIntegral g2)
                (round $ m * fromIntegral b1 + (1-m) * fromIntegral b2)
  return $ mix m lColor bgColor
