
{-# LANGUAGE StandaloneDeriving #-}

module GUI.SourceView (
  SourceView,
  sourceViewNew,
  SourceViewActions(..),

  Tag,

  sourceViewSetEvents,

  sourceViewClear,
  sourceViewAdjustSelection,
  sourceViewSetSelection,
  ) where

import Events.Core as Core
import Events.Debug
import Events.HECs          (timestampToEventIndex)
import GUI.Timeline.Types
import GUI.Types            (Options(..))

import GHC.RTS.Events

import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView hiding (SourceView, sourceViewNew)
import qualified Graphics.UI.Gtk.SourceView as GtkSourceView
import Graphics.Rendering.Cairo

import Data.Array
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import Data.Char (chr, ord, isSpace, isAlphaNum)
import qualified Data.Function as F
import Data.IORef
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Monoid (mappend)
import Data.List
import qualified Data.Set as Set
import Data.Tree (Tree(Node))
import Data.Word (Word64)
import Data.Ord (comparing)

import System.FilePath
import System.Directory (doesFileExist,getCurrentDirectory,canonicalizePath)

import Control.Monad (forM_, forM, when)
import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.DeepSeq (deepseq, NFData(..))
import Control.Exception (catch, IOException)

import Text.Printf

import Paths_threadscope_pmw (getDataFileName)

-------------------------------------------------------------------------------

data FileView = FileView {
  sourceFile     :: FilePath,
  sourceLoad     :: IORef Bool,
  sourcePage     :: Int,
  sourceView     :: GtkSourceView.SourceView,
  sourceBuffer   :: SourceBuffer,
  sourceScroll   :: ScrolledWindow,
  sourceOverview :: DrawingArea,
  sourceTextTags :: IORef [TextTag]
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
  srcTagsStore    :: TreeStore SourceTag,
  sampleChooser   :: ComboBox,
  sampleChoiceStore :: ListStore SampleVerb,
  structRenderer  :: CellRendererPixbuf,
  globalSearchDirs:: [FilePath],

  textHeight   :: !Int
  }

type EventsArray = Array Int CapEvent

data SourceViewState
  = SourceViewState {
    eventsArr  :: EventsArray,
    dbgMap     :: DebugMaps,
    selStart   :: Timestamp,
    selEnd     :: Timestamp,
    sampleType :: SampleVerb,
    tags       :: [Tag],
    sourceTags :: [SourceTag],
    fileTags   :: [SourceTag],
    selection  :: Maybe Tag,
    srcSel     :: Maybe SourceTag,
    hintSel    :: [Tag],
    openEntries:: [DebugEntry],
    files      :: [FileView],
    searchDirs :: [FilePath],
    lineTags   :: [[Tag]],
    structBufMap :: !StructPixbufMap
  }

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
  stagFile :: String,
  stagName :: String,
  stagSources :: [Span],
  stagTags :: [Tag],
  stagFreq :: !Double
  }

instance Eq Tag where
  (Tag m1 n1 t1 _ _ _) == (Tag m2 n2 t2 _ _ _)  = m1 == m2 && (n1 == n2 || (t1 /= Nothing && t1 == t2))

instance Ord Tag where
  (Tag m1 n1 t1 _ _ _) `compare` (Tag m2 n2 t2 _ _ _) =
    (m1 `compare` m2) `mappend` (n1 `compare` n2) `mappend` (t1 `compare` t2)

instance NFData Tag where
  rnf Tag{..} = tagUnit `deepseq`
                tagName `deepseq`
                tagTick `deepseq`
                tagDebug `seq` ()

initViewState :: SourceViewState
initViewState = SourceViewState {
  eventsArr = listArray (0,0) [],
  dbgMap = emptyDebugMaps,
  selStart = 0,
  selEnd = 0,
  sampleType = SampleByCycle,
  tags = [],
  sourceTags = [],
  fileTags = [],
  selection = Nothing,
  srcSel = Nothing,
  hintSel = [],
  files = [],
  searchDirs = [],
  lineTags = [],
  openEntries = [],
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


bsToStr :: BS.ByteString -> String
bsToStr = map (chr.fromIntegral) . BS.unpack

dumpTag :: Tag -> IO ()
dumpTag tag = do
  putStr $ printf "%02.2f" (100 * tagFreq tag) ++ "% "
  case tagDebug tag of
    Just dbg -> do
      let subs = getSubsumationEntry dbg
      if subs /= dbg
        then putStr $ " (part of " ++ bsToStr (dbgUnit subs) ++ "/" ++ bsToStr (dbgLabel subs) ++ "): "
        else putStr ": "
      dumpDebug dbg
    Nothing  -> putStrLn $ fromMaybe "no unit" (tagUnit tag) ++ "/" ++
                           fromMaybe "no name" (tagName tag) ++ ": no dbg..."

sourceViewNew :: Builder -> Options -> SourceViewActions -> IO SourceView
sourceViewNew builder opts SourceViewActions{..} = do

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

  -- Set "Core" label as bold
  coreLabel <- getWidget castToLabel "core_label"
  boldFont <- fontDescriptionNew
  fontDescriptionSetWeight boldFont WeightBold
  widgetModifyFont coreLabel $ Just boldFont

  -- Lookup mark icons
  sourceViewSetMarkCategoryIconFromStock coreView coreMarkCatFolded (Just stockAdd)
  sourceViewSetMarkCategoryIconFromStock coreView coreMarkCatOpen (Just stockRemove)
  sourceViewSetMarkCategoryIconFromStock coreView coreMarkCatOpenEnd (Just "") -- "Nothing" causes Gtk-Critical
  sourceViewSetMarkCategoryIconFromStock coreView coreMarkCatUp (Just stockGoUp)
  sourceViewSetMarkCategoryIconFromStock coreView coreMarkCatUpEnd (Just "") -- dito

  -- Find some layout parameters
  (wdt, textHeight) <- getTextDimensions coreView "100.0%"

  -- Find style data
  style <- widgetGetStyle coreView
  bgColor <- styleGetBackground style StateNormal

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

  -- Initialize state
  stateRef <- newIORef initViewState

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
    [ cellText := fromMaybe "" (bsToStr . dbgCoreBind <$> findDbgElem dbgDCore tagDebug) ]
  cellLayoutSetAttributes tagModCol tagModRender tagsStore $ \Tag{..} ->
    [ -- If the compilation unit is "SYMTAB", this is a pseudo
      -- module inserted by GHC for procedure ranges it found but
      -- couldn't map to a piece of Haskell code. We show the name
      -- of the binary the symbol came from in that case.
      cellText := case fmap dbgUnit tagDebug of
            Just m | symTabPrefix `isPrefixOf` bsToStr m
                     -> "(" ++ takeFileName (drop (length symTabPrefix) $ bsToStr m) ++ ")"
                   | otherwise
                     -> takeFileName (bsToStr m)
            Nothing  -> "(?)"
    ]
  cellLayoutSetAttributes tagDescCol tagDescRender tagsStore $ \Tag{..} ->
    [ cellText := case findDbgElem dbgDName tagDebug of
          -- We either get the name from an annotation, or we use the
          -- label used for the procedure in the binary symbol table
         Just n -> bsToStr n
         _ | Just l <- fmap dbgLabel tagDebug
                -> "(" ++ zdecode (bsToStr l) ++ ")"
           | otherwise
                -> "(?)"
    ]

  -- Create columns for source tag tree
  srcTagsTreeView  <- getWidget castToTreeView "source_tagstree1"
  srcTagsStore     <- treeStoreNew []

  srcTagFreqRender <- cellRendererProgressNew
  srcTagFreqCol    <- mkColumn srcTagsTreeView srcTagFreqRender "%"
  srcTagNameRender <- cellRendererTextNew
  srcTagNameCol    <- mkColumn srcTagsTreeView srcTagNameRender "Name"

-- updateSourceTagsActivation :: TreeStore (SourceTag, Bool) -> SourceTag -> IO ()
-- updateSourceTagsActivation tagsStore tag = do
--   treeModelForeach tagsStore $ \i -> do
--     p <- treeModelGetPath tagsStore i
--     treeStoreChange tagsStore p $ \(t, _) ->
--       (t, any (`elem` stagTags tag) $ stagTags t)

  -- Set column content
  treeViewSetModel srcTagsTreeView srcTagsStore
  let checkHint tag = do
        SourceViewState{hintSel} <- readIORef stateRef
        return $ any (`elem` hintSel) $ stagTags tag
  cellLayoutSetAttributeFunc srcTagFreqCol srcTagFreqRender srcTagsStore $ \iter -> do
    tag <- treeModelGetRow srcTagsStore iter
    hint <- checkHint tag
    set srcTagFreqRender
      [ cellProgressText := Just $ printf "%02.1f" (stagFreq tag * 100)
      , cellProgressValue := round (stagFreq tag * 100)
      , cellBackgroundColor := bgColor
      , cellBackgroundSet := hint ]
  cellLayoutSetAttributeFunc srcTagNameCol srcTagNameRender srcTagsStore $ \iter -> do
    tag <- treeModelGetRow srcTagsStore iter
    hint <- checkHint tag
    set srcTagNameRender
      [ cellText := stagName tag
      , cellBackgroundColor := bgColor
      , cellBackgroundSet := hint ]
    return ()

  -- Set up search column
  treeViewSetSearchColumn srcTagsTreeView (makeColumnIdString 1)
  treeViewSetEnableSearch srcTagsTreeView True

  -- Get sample type chooser
  sampleChooser <- getWidget castToComboBox "sample_type_chooser"
  sampleChoiceStore <- listStoreNew [SampleByCycle, SampleByHeap, SampleByLifeHeap]

  sampleChoiceRender <- cellRendererTextNew
  cellLayoutPackStart sampleChooser sampleChoiceRender True
  cellLayoutSetAttributeFunc sampleChooser sampleChoiceRender sampleChoiceStore $ \iter -> do
    choice <- treeModelGetRow sampleChoiceStore iter
    set sampleChoiceRender
      [ cellText := case choice of
           SampleByCycle -> "By CPU Cycles"
           SampleByHeap  -> "By Allocation"
           SampleByLifeHeap -> "By Heap Data" ]
    return ()

  comboBoxSetModel sampleChooser (Just sampleChoiceStore)
  Just firstEntry <- treeModelGetIterFirst sampleChoiceStore
  comboBoxSetActiveIter sampleChooser firstEntry

  let globalSearchDirs = optSearchPaths opts
      srcView    = SourceView {..}

  -- Register events
  on tagsTreeView cursorChanged $
    updateTagSelection srcView
  on srcTagsTreeView cursorChanged $
    updateSrcTagSelection srcView
  on sourceBook switchPage $
    updateFileView srcView
  after coreView sourceViewLineMarkActivated $ \pos ->
    liftIO $ activateMark srcView coreBuffer pos
  on coreBuffer markSet $ \iter mark -> do
    markName <- textMarkGetName mark
    when (markName == Just "insert") $ do
      state@SourceViewState{lineTags} <- readIORef stateRef
      -- Get top-level tag at given line
      line <- textIterGetLine iter
      case drop line lineTags of
        ((tag:_):_) -> do
          writeIORef stateRef state{hintSel=[tag]}
          widgetQueueDraw srcTagsTreeView
        _other -> return ()
  on sampleChooser changed $ do
    miter <- comboBoxGetActiveIter sampleChooser
    case miter of
      Just iter -> do
        sampleType <- treeModelGetRow sampleChoiceStore iter
        modifyIORef stateRef $ \state -> state { sampleType = sampleType }
        loadTags srcView
      Nothing -> return ()

  -- Set up tooltip. queryTooltip doesn't seem to work, so
  -- we use some lame hack using selection.
  set coreView [ widgetHasTooltip := True
               , widgetTooltipText := Just "Test"
               ]
  let updateTooltip = do

        -- get gutter width
        costRenderWdt <- get costRenderer cellWidth
        structRenderWdt <- get structRenderer cellWidth
        let totalWdt = costRenderWdt + structRenderWdt + 20

        -- get iter at position
        (x,y) <- widgetGetPointer coreView
        (bx,by) <- textViewWindowToBufferCoords coreView TextWindowText
                   (x-totalWdt, y)
        iter <- textViewGetIterAtLocation coreView bx by
        -- backward to word start, forward to word end
        updateCoreTooltip srcView iter
        return True
  timeoutAdd updateTooltip 1000

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

sourceViewSetEvents :: SourceView -> Maybe FilePath -> Maybe (EventsArray, DebugMaps) -> IO ()
sourceViewSetEvents SourceView{..} m_file m_data = do
  writeIORef stateRef =<< case (m_file, m_data) of
    (Just file, Just (eventsArr, dbgMap)) -> do

      -- Search dirs
      curDir <- getCurrentDirectory
      searchDirs <- nub <$> mapM canonicalizePath (
        globalSearchDirs ++ [takeDirectory file, curDir])

      -- Set initial tags
      return initViewState {
        eventsArr = eventsArr,
        dbgMap = dbgMap,
        tags = tagsFromLocalIPs2 0 eventsArr dbgMap,
        searchDirs = searchDirs
        }

    _other -> return initViewState

------------------------------------------------------------------------------

searchGen :: (FilePath -> IO Bool) -> [FilePath] -> FilePath -> IO (Maybe FilePath)
searchGen _    []         _    = return Nothing
searchGen test (dir:dirs) name = do
  full_name <- Control.Exception.catch (canonicalizePath (combine dir name))
               (const (return name) :: IOException -> IO FilePath)
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
fileViewNew SourceView {stateRef,haskellLang,sourceFont,sourceBook} sourceFile = do

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
  scrolledWindowSetPolicy sourceScroll PolicyAutomatic PolicyAlways
  containerAdd sourceScroll sourceView

{--
  -- Create overview drawing area
  sourceOverview <- drawingAreaNew
  widgetSetSizeRequest sourceOverview 20 20

  -- Create HBox for placing scrolled window and overview next to each other
  hBox <- hBoxNew False 1
  boxPackStart hBox sourceScroll PackGrow 0
  boxPackEnd hBox sourceOverview PackNatural 0
--}

  let hBox = sourceScroll
      sourceOverview = undefined

  -- Create label with full path
  sourceLabel <- labelNew (Just sourceFile)
  miscSetAlignment sourceLabel 0 0
  boldFont <- fontDescriptionNew
  fontDescriptionSetWeight boldFont WeightBold
  widgetModifyFont sourceLabel $ Just boldFont

  -- Add above source scroll
  vBox <- vBoxNew False 1
  boxPackStart vBox sourceLabel PackNatural 0
  boxPackEnd vBox hBox PackGrow 0

  -- Create notebook page for the file
  sourcePage <- notebookAppendPage sourceBook vBox $ takeFileName sourceFile

  -- We don't actually load any content yet
  sourceLoad <- newIORef False
  sourceTextTags <- newIORef []

  -- Add to file view list
  let fileView = FileView {..}
  modifyIORef stateRef $ \state ->
    state { files = files state ++ [fileView] }

{--
  -- Register for drawing overview
  on sourceOverview exposeEvent $ do
    region <- eventRegion
    liftIO (drawFileOverview sview fileView region)
    return True
--}
  -- Show everything
  widgetShowAll vBox

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

sourceViewClear :: SourceView -> IO ()
sourceViewClear view@SourceView{..} = do

  -- Clear
  modifyIORef stateRef $ \ref -> ref {
    tags = [], sourceTags = [], fileTags = [],
    selection = Nothing, srcSel = Nothing, hintSel = []
    }
  postGUIAsync (updateViews view)

sourceViewAdjustSelection :: SourceView -> TimeSelection -> IO (Maybe TimeSelection)
sourceViewAdjustSelection  _             RangeSelection{}     = return Nothing
sourceViewAdjustSelection SourceView{..} (PointSelection sel) = do

  -- We can't really work with a point selection, so let's "enhance"
  -- the selection until we have a "good" amount of samples
  let goodAmountOfSamples = 100

  SourceViewState{eventsArr} <- readIORef stateRef
  let countSamples CapEvent{ce_event=Event{spec=InstrPtrSample{ips}}}
                     = snd $ UA.bounds ips
      countSamples _ = 0
      (_, bound)     = bounds eventsArr
      findEnd i !samples
        | i > bound                      = bound
        | samples' > goodAmountOfSamples = i
        | otherwise                      = findEnd (i+1) samples'
        where samples' = samples + countSamples (eventsArr ! i)

      end = findEnd (timestampToEventIndex eventsArr sel) 0
      endTime = time $ ce_event $ eventsArr ! end

  putStrLn $ "End: " ++ show end
  putStrLn $ "Extending selection until " ++ show endTime

  return $ Just $ RangeSelection sel $! endTime

sourceViewSetSelection :: SourceView -> TimeSelection -> IO ()
sourceViewSetSelection view@SourceView{stateRef} timeSel = do

  let (start, end) = case timeSel of
        PointSelection start -> (start, start)
        RangeSelection start end -> (start, end)

  modifyIORef stateRef $ \state -> state {
    selStart = start,
    selEnd = end }

  loadTags view

loadTags :: SourceView -> IO ()
loadTags view@SourceView{..} = do

  state@SourceViewState{..} <- readIORef stateRef

  -- Load tags for new position
  let tags' = tagsByInterval sampleType eventsArr dbgMap (selStart, selEnd)

  -- Update selection, if possible
  let selection' = selection >>= (\t -> find (== t) tags')

  -- Build source tag list
  let getSources tag =
        case map (mkSourceTag tag) $ findDebugSources $ tagDebug tag of
          [] -> [mkNoSourceTag tag]
          xs -> xs
      mkSourceTag tag src = SourceTag { stagFile = bsToStr $ fileName src
                                      , stagName = bsToStr $ spanName src
                                      , stagSources = [src]
                                      , stagTags = [tag]
                                      , stagFreq = tagFreq tag }
      mkNoSourceTag tag = SourceTag { stagFile = if "stg_" `isPrefixOf` fromMaybe "" (tagName tag) 
                                                 then "(STG)"
                                                 else "(no haskell)"
                                    , stagName = maybe "?" (\x -> "("++x++")") $ tagName tag
                                    , stagSources = []
                                    , stagTags = [tag]
                                    , stagFreq = tagFreq tag }
      sourceTags' = sortBy (compare `F.on` stagFreq) $
                    subsumeSrcTags False $ concatMap getSources tags'
      fileTags' = sortBy (compare `F.on` stagFreq) $
                  map mkFileSrcTag $
                  subsumeSrcTags True $ concatMap getSources tags'
      mkFileSrcTag s
        = s { stagName = stagFile s }

  -- Update source tag selection
  let srcSel' = srcSel >>= (\s -> find (sourceTagEquiv s) sourceTags')

  -- Set new state
  tags' `deepseq`
    writeIORef stateRef state{
      tags=tags', sourceTags=sourceTags', fileTags=fileTags',
      selection=selection', srcSel=srcSel', hintSel=maybe [] stagTags srcSel'
      }

  -- Update views
  postGUIAsync (updateViews view)

-- | Updates the whole view after a selection change
updateViews :: SourceView -> IO ()
updateViews view@SourceView{..} = do

  SourceViewState{tags, sourceTags, fileTags, selection, srcSel} <- readIORef stateRef
  updateTagsView tagsStore tags
  updateSourceTagsView srcTagsStore sourceTags fileTags
  updateLineTags view
  setTagSelection view selection
  setSrcTagSelection view srcSel
  updateStructPixbufMap view


-- | If the debug entry has no source annotations at all, we assume
-- that the source code annotations of the paraent context are
-- most relevant (if any).
findDebugSources :: Maybe DebugEntry -> [Span]
findDebugSources Nothing       = []
findDebugSources (Just DebugEntry{dbgSources, dbgParent})
  | not (null dbgSources) = dbgSources
  | otherwise             = findDebugSources dbgParent


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
        = map unitTree $ reverse $ filter ((== file) . stagFile) tags
  treeStoreClear tagsStore
  treeStoreInsertForest tagsStore [] 0 $ reverse
    [ Node st (stagForest $ stagFile st) | st <- fileTags ]

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
                      Just dbg -> map (bsToStr . fileName) $ extSources dbg
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
            Just dbg -> filter ((== sourceFile f) . bsToStr . fileName) $ extSources dbg
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
    srcSel = select',
    hintSel = maybe [] stagTags select',
    openEntries = nub . map getSubsumationEntry .
                  mapMaybe tagDebug $ maybe [] stagTags select'
    }

  case select' of
    Just stag -> do

      -- Find file to show
      let unit = stagFile stag
      fileView <- fileViewGet view unit
      notebookSetCurrentPage sourceBook (sourcePage fileView)
      updateTextTags view fileView

      -- Scroll to function
      -- Note: We are creating a mark here, as scrolling to an
      -- iterator position is not reliable with freshly opened text
      -- buffers.
      -- Note 2: This still doesn't work, in fact
      case stagSources stag of
        (span:_) -> do
          let buf = sourceBuffer fileView
          textIter <- textBufferGetIterAtLineOffsetSafe buf
                  (startLine span) (startCol span)
          let markName = "threadscope-scroll-mark"
          m_scrollMark <- textBufferGetMark buf markName
          scrollMark <- case m_scrollMark of
            Just mark -> do textBufferMoveMark buf mark textIter
                            return mark
            Nothing   -> textBufferCreateMark buf (Just markName) textIter True
          textViewScrollToMark (sourceView fileView) scrollMark 0 (Just (0.5,0.5))
        _ -> return ()

      -- Update labels
      updateFileBookLabels view

      -- Subsume tags
      let tags = subsumeTagFamilies $ subsumeTags $ stagTags stag

      -- Show cores
      clearCore view
      forM_ (reverse $ sortBy (compare `F.on` tagFreq) tags) $ \tag -> do
        iter <- textBufferGetEndIter coreBuffer
        writeSource view iter [] $ "---- " ++ fromMaybe "???" (tagName tag) ++ " ----\n"
        showCore view tag
        iter <- textBufferGetEndIter coreBuffer
        writeSource view iter [] "\n"

      -- update activation due to new selection
      widgetQueueDraw srcTagsTreeView


    Nothing ->
      return ()


setSrcTagSelection :: SourceView -> Maybe SourceTag -> IO ()
setSrcTagSelection SourceView{..} Nothing = do
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
sourceTagEquiv st1 st2
  = stagFile st1 == stagFile st2 && stagName st1 == stagName st2


------------------------------------------------------------------------------

{--
findLocalEvents :: Int -> Timestamp -> EventsArray -> (CapEvent -> Maybe a) -> [a]
findLocalEvents n winSize eventsArr filter_f =
  let winMid        = time $ ce_event $ eventsArr ! n
      eventsWhile f = takeWhile (f.time.ce_event) . map (eventsArr !)
      (low, high)   = bounds eventsArr
      eventsBefore  = eventsWhile (>winMid-winSize) [n-1,n-2..low]
      eventsAfter   = eventsWhile (<winMid+winSize) [n,n+1..high]
  in mapMaybe filter_f eventsBefore ++ mapMaybe filter_f eventsAfter
--}

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

-- | Weight to give to a sample. Simply the normal distribution.
sampleWeight :: Timestamp -> Timestamp -> Timestamp -> Double
sampleWeight mid dev val =
  exp (fromIntegral ((val - mid) ^ 2) / fromIntegral (dev * dev))

{--

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
--}

-------------------------------------------------------------------------------

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

findLocalIPsamples :: Int -> EventsArray -> [(Timestamp, [Word64])]
findLocalIPsamples startIx eventsArr =
  let getIPs CapEvent{{-ce_cap,-}ce_event=Event{time,spec=InstrPtrSample{..}}}
        = Just ({- ce_cap, -}time, UA.elems ips)
--      getIPs CapEvent{{-ce_cap,-}ce_event=Event{time,spec=Blackhole{..}}}
--        = Just (time, [ip])
      getIPs _other
        = Nothing
      count_f (_, ips) = length ips
      samples = findLocalEventsCount startIx ipSampleMaxCount eventsArr getIPs count_f
  in samples

-- | Removes duplicates and allows for custom combination function
nubSumBy :: (a -> a -> Ordering) -> (a -> a -> a) -> [a] -> [a]
nubSumBy ord plus = map (foldr1 plus) . groupBy eq . sortBy ord
  where x `eq` y = ord x y == EQ

-- | Finds local IP samples, return weighted
findLocalIPsWeighted :: Int -> EventsArray -> DebugMaps -> [(Double, Maybe DebugEntry)]
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

tagsFromLocalIPs2 :: Int -> EventsArray -> DebugMaps -> [Tag]
tagsFromLocalIPs2 startIx eventsArr rangeMap =
  let toTag freq (Just dbg)        = tagFromDebug freq dbg
      toTag freq Nothing
        = let tag = Tag { tagUnit = Nothing
                        , tagName = Just "-unrecognized-"
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

-------------------------------------------------------------------------------

eventsByInterval :: EventsArray -> (Timestamp, Timestamp) -> (CapEvent -> Maybe a) -> [a]
eventsByInterval eventsArr (start, end) filter_f = go startIx
  where startIx = timestampToEventIndex eventsArr start
        endIx = min (timestampToEventIndex eventsArr end + 1)
                    (snd $ bounds eventsArr)
        go i | i >= endIx = []
             | Just x <- filter_f (eventsArr ! i)
                          = x : go (i+1)
             | otherwise  = go (i+1)

samplesByInterval :: SampleVerb -> EventsArray -> (Timestamp, Timestamp) -> [(Timestamp, [(Int, Word64)])]
samplesByInterval stype eventsArr range =
  let getSamples CapEvent{{-ce_cap,-}ce_event=Event{time,spec=Samples{..}}}
        | sample_type == SampleInstrPtr && sample_by == stype
        = Just ({- ce_cap, -}time, zip (map fromIntegral $ UA.elems weights) (UA.elems samples))
      getSamples _other
        = Nothing
      samples = eventsByInterval eventsArr range getSamples
  in samples

{--
-- | Returns a list of GCs that started in the given time frame. If
-- there is a GC that started in the time interval, but ended later,
-- it will be truncated.
findGCs :: EventsArray -> (Timestamp, Timestamp) -> [(Timestamp, Timestamp)]
findGCs eventsArr (start, end) = go_start startIx
  where startIx = timestampToEventIndex eventsArr start
        endIx = min (timestampToEventIndex eventsArr end)
                    (snd $ bounds eventsArr)
        go_start !i
          | i > endIx
             = []
          | is_gc_start i
             = go_end (i+1) (event_time i)
          | otherwise
             = go_start (i+1)
        go_end !i !time
          | i > endIx
             = if time < end
               then [(time, end)]
               else []
          | is_gc_end i
             = if i < startIx
               then go_start (i+1)
               else (time, event_time i) : go_start (i+1)
          | otherwise
             = go_end (i+1) time
        event_time i = case eventsArr ! i of
          CapEvent{ce_event=Event{time}} -> time
        is_gc_start i = case eventsArr ! i of
          CapEvent{ce_event=Event{spec=StartGC{..}}}
             -> True
          _  -> False
        is_gc_end i = case eventsArr ! i of
          CapEvent{ce_event=Event{spec=EndGC{..}}}
             -> True
          _  -> False
--}
weightedSamplesByInterval :: SampleVerb -> EventsArray -> DebugMaps -> (Timestamp, Timestamp)
                             -> [(Double, Maybe DebugEntry)]
weightedSamplesByInterval sampleType eventsArr rangeMap interval = weighted
  where samples    = samplesByInterval sampleType eventsArr interval
        samples'   = concatMap snd samples
        resolved   = lookupRanges rangeMap samples'
        findAlc (n,d) = (n, fmap (findAllocationEntry rangeMap) d)
        corrected  | sampleType == SampleByHeap  = map findAlc resolved
                   | otherwise                   = resolved
        baseWeight = 1 / fromIntegral (sum $ map fst corrected)
        weightCycle (n, dbg) = (fromIntegral n * baseWeight, dbg)
        weighted = map weightCycle corrected

{--
weightedMixedSamplesByInterval :: EventsArray -> DebugMaps -> (Timestamp, Timestamp)
                                  -> [(Double, Maybe DebugEntry)]
weightedMixedSamplesByInterval eventsArr rangeMap interval =
    trace ("GC: " ++ show gcTime ++ " Tot: " ++ show totTime) $
    trace ("Cycle sample points: " ++ show (length cycleSamples) ++ " Heap: " ++ show (length heapSamples) ) $
    trace ("Cycle base weight: " ++ show cycleBaseWeight ) $
    trace ("Mutator fraction: " ++ show muFract) $
    trace ("Cycle weight sum: " ++ show (sum $ map fst cycleWeighted)) $
    trace ("Heap weight sum: " ++ show (sum $ map fst heapWeighted)) $
    cycleWeighted ++ heapWeighted
  where -- Data from eventlog
        gcs          = findGCs eventsArr interval
        cycleSamples = samplesByInterval SampleByCycle eventsArr interval
        heapSamples  = samplesByInterval SampleByHeap eventsArr interval

        -- Cycle samples are all weighted according to mutator time in
        -- interval
        cycleSamples' = concatMap snd cycleSamples
        cycleBaseWeight = muFract / fromIntegral (length cycleSamples')
        weightCycle (n, dbg) = (fromIntegral n * cycleBaseWeight, dbg)
        cycleWeighted = map weightCycle $ lookupRanges rangeMap cycleSamples'

        -- Heap samples are weighted by length of next GC from sample
        -- point
        heapWeighted = go_hp gcs heapSamples
        go_hp :: [(Timestamp, Timestamp)] -> [(Timestamp, [Word64])] -> [(Double, Maybe DebugEntry)]
        go_hp [] _ = []
        go_hp _ [] = []
        go_hp ((start_gc, end_gc):gcs) samples
          = let -- Collect all samples happening before this GC
                -- (might be none - we lose some cost then!)
                (sss, samples') = span (\(time,_) -> time < start_gc) samples
                ss = concatMap snd sss
                -- Weight them according to GC length and number of samples found
                !w = (fromIntegral (end_gc - start_gc)) / fromIntegral totTime / fromIntegral (length ss)
                weight (n, dbg) = (fromIntegral n * w, dbg)
                -- Lookup debug info
                dbgs = lookupRanges rangeMap ss
            in trace ("Weight " ++ show w ++ " for " ++ show (length ss) ++ "samples " ++
                      "from " ++ show start_gc ++ " to " ++ show end_gc) $
               map weight dbgs ++ go_hp gcs samples'

        -- We assume here that the time interval is split exactly
        -- between GC and "mutator" (to which we might count C and
        -- RTS) which is obviously ignoring a few cases.
        dist (s,e) = e - s
        totTime = dist interval
        gcTime  = sum $ map dist gcs
        muFract = fromIntegral (totTime - gcTime) / fromIntegral totTime
 --}

tagsByInterval :: SampleVerb -> EventsArray -> DebugMaps -> (Timestamp, Timestamp) -> [Tag]
tagsByInterval sampleType eventsArr rangeMap interval =
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

      weighted = weightedSamplesByInterval sampleType eventsArr rangeMap interval
      grandSum = sum $ map fst weighted
      tags = map (uncurry toTag . first (/grandSum)) weighted

      ord = compare `F.on` (fmap dbgId . tagDebug)
      t1 `plus` t2 = t1 { tagFreq = tagFreq t1 + tagFreq t2 }
  in nubSumBy ord plus tags


-------------------------------------------------------------------------------

tagFromDebug :: Double -> DebugEntry -> Tag
tagFromDebug freq dbg@DebugEntry {..}
  = let tag = Tag { tagUnit = Just $ bsToStr dbgUnit
                  , tagName = Just $ zdecode $ bsToStr dbgLabel
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

clearTextTags :: FileView -> IO ()
clearTextTags FileView{sourceBuffer, sourceTextTags} = do
  tagList <- readIORef sourceTextTags
  tagTable <- textBufferGetTagTable sourceBuffer
  mapM_ (textTagTableRemove tagTable) tagList
  writeIORef sourceTextTags []

updateTextTags :: SourceView -> FileView -> IO ()
updateTextTags SourceView{..} fw@FileView{..}= do

  -- Clear existing tags
  clearTextTags fw

  -- Annotate source code (all tags, then specificially the selection)
  SourceViewState{srcSel} <- readIORef stateRef
  let annotate = annotateTags sourceFile sourceView sourceBuffer sourceTextTags
  case srcSel of
    Nothing  -> return ()
    Just sel -> do
      annotate (concatMap (findDebugSources . tagDebug) $ stagTags sel) False
      annotate (stagSources sel) True

{--
-- | From a list of tags, gives the source ranges that are covered by
-- the tags in the given file.
getFileSourceSpans :: String -> [Tag] -> [(Double, Span)]
getFileSourceSpans sourceFile tags =
  let spans = [(tagFreq, src)
              | Tag {tagFreq, tagDebug = Just dbg} <- tags
              , src <- extSources dbg
              , fileName src == sourceFile ]
   in nubSumBy (compare `F.on` snd) (\(f1, _) (f2, t) -> (f1+f2,t)) spans
--}

  -- "Safe" version of textBufferGetIterAtLineOffset
textBufferGetIterAtLineOffsetSafe :: TextBufferClass self => self -> Int -> Int -> IO TextIter
textBufferGetIterAtLineOffsetSafe buf l c = do
  lineCount <- textBufferGetLineCount buf
  if l >= lineCount
    then textBufferGetEndIter buf
    else do iter <- textBufferGetIterAtLine buf l
            textIterForwardChars iter c
            return iter

annotateTags :: String -> GtkSourceView.SourceView -> SourceBuffer -> IORef [TextTag] -> [Span] -> Bool -> IO ()
annotateTags sourceFile sourceView sourceBuffer sourceTextTags spans sel = do
  tagTable <- textBufferGetTagTable sourceBuffer

  -- Find colors
  style <- widgetGetStyle sourceView
  bgColor <- styleGetBackground style $
             if sel then StateSelected else StateNormal

  -- Filter spans by file
  let spans' = filter ((== sourceFile) . bsToStr . fileName) spans

  -- Get spans to annotate
  ttags <- forM spans' $ \Span {..} -> do

    -- Create color tag
    tag <- textTagNew Nothing
    set tag [textTagBackgroundGdk := bgColor, textTagBackgroundSet := True]
    tagTable `textTagTableAdd` tag

      -- Place at code position
    start <- textBufferGetIterAtLineOffsetSafe sourceBuffer (startLine-1) (startCol-1)
    end <- textBufferGetIterAtLineOffsetSafe sourceBuffer (endLine-1) endCol
    textBufferApplyTag sourceBuffer tag start end

    --putStrLn $ "Annotating " ++ show (sl, sc, el, ec) ++ " with " ++ clr ++ "(lvl " ++ show lvl ++ ")"

    return tag

  -- Save back text tags
  modifyIORef sourceTextTags (++ ttags)

-------------------------------------------------------------------------------

coreMarkCatFolded, coreMarkCatOpen, coreMarkCatOpenEnd :: String
coreMarkCatUp, coreMarkCatUpEnd :: String
coreMarkCatFolded  = "threadscope-core-mark-folded"
coreMarkCatOpen    = "threadscope-core-mark-open"
coreMarkCatOpenEnd = "threadscope-core-mark-open-end"
coreMarkCatUp      = "threadscope-core-mark-up"
coreMarkCatUpEnd   = "threadscope-core-mark-up-end"

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
  sourceBufferRemoveSourceMarks coreBuffer begin end coreMarkCatUp
  sourceBufferRemoveSourceMarks coreBuffer begin end coreMarkCatUpEnd
  textBufferDelete coreBuffer begin end

showCore :: SourceView -> Tag -> IO ()
showCore sview@SourceView{coreBuffer,stateRef} tag = do

  -- Collect information about stuff to show
  SourceViewState{tags} <- readIORef stateRef
  let dbg = findWithDbgElem dbgDCore $ tagDebug tag
      ltags = case dbg of
        Just d  -> [tagByCore tags d]
        Nothing -> []

  -- Write data to buffer
  iter <- textBufferGetEndIter coreBuffer
  case tagUnit tag of
    Just unit | Just core <- fmap dbgCoreCode (dbg >>= dbgDCore)
      -> printTopLevel sview iter unit dbg ltags core
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

-- | Writes top-level core, including an "up" link if appropriate
printTopLevel :: SourceView -> TextIter -> String -> Maybe DebugEntry -> [Tag] -> CoreExpr -> IO ()
printTopLevel sview iter unit dbg ltags core
  = case findDbgElem dbgDCore (dbgParent =<< dbg) of
      Just parent -> do
        let parent_bind = (bsToStr $ dbgCoreBind parent,
                           bsToStr $ dbgCoreCons parent)
        writeUpMarkStart sview iter unit parent_bind
        printCore sview iter unit ltags core
        writeUpMarkEnd sview iter unit parent_bind
      Nothing ->
        printCore sview iter unit ltags core

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

-- | Marks the given position as the start of an "up" context navigator
writeUpMarkStart :: SourceView -> TextIter -> String -> (String, String) -> IO ()
writeUpMarkStart SourceView{coreBuffer} iter unit ref = do
  sourceBufferCreateSourceMark coreBuffer
    (Just $ markNameStart unit ref) coreMarkCatUp iter >> return ()

-- | Marks the given position as the end of an "up" context navigator
writeUpMarkEnd :: SourceView -> TextIter -> String -> (String, String) -> IO ()
writeUpMarkEnd SourceView{coreBuffer} iter unit ref  =
  sourceBufferCreateSourceMark coreBuffer
    (Just $ markNameEnd unit ref) coreMarkCatUpEnd iter >> return ()

-- | Creates a (possibly subsumed) tag for the core piece in the debug
-- entry. This is necessary as we might have multiple tags for the
-- same piece of core.
tagByCore :: [Tag] -> DebugEntry -> Tag
tagByCore tags core_dbg =
  let same_core = (== Just core_dbg) . findWithDbgElem dbgDCore . tagDebug
  in case filter same_core tags of
    []        -> tagFromDebug 0 core_dbg
    core_tags ->
      let top_tag = case find ((== Just core_dbg) . tagDebug) tags of
            Just t  -> t
            Nothing -> head core_tags
      in top_tag { tagFreq = sum (map tagFreq core_tags) }

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
  SourceViewState{dbgMap,tags,openEntries} <- readIORef stateRef
  case lookupCore dbgMap unit bind cons of
    Nothing
      -> writeSource sview iter tags $ "#ref " ++ unit ++ "/" ++ bind ++ "/" ++ cons ++ "!#"

    -- Subsumed core piece? Generate open fold
    Just cdbg | Just core <- dbgDCore cdbg,
                sub <- getSubsumationEntry cdbg,
                sub `elem` openEntries
      -> do let ltags' = tagByCore tags cdbg : ltags
            writeOpenMarkStart sview iter unit (bind, cons)
            printCore sview iter unit ltags' (dbgCoreCode core)
            writeOpenMarkEnd sview iter unit (bind, cons)

    -- Otherwise generate closed fold
    Just cdbg
      -> do let ltags' = tagByCore tags cdbg : ltags
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

-- | Called when a mark gets activated.
activateMark :: SourceView -> SourceBuffer -> TextIter -> IO ()
activateMark sview@SourceView{stateRef} coreBuffer pos = do

  -- Get our marks
  line <- textIterGetLine pos
  foldedMarks <- sourceBufferGetSourceMarksAtLine coreBuffer line coreMarkCatFolded
  openMarks <- sourceBufferGetSourceMarksAtLine coreBuffer line coreMarkCatOpen
  upMarks <- sourceBufferGetSourceMarksAtLine coreBuffer line coreMarkCatUp

  -- Lookup tags at position
  SourceViewState{lineTags=ltagss,tags,dbgMap} <- readIORef stateRef
  let ltags = case drop line ltagss of
        (ltags:_) -> ltags
        _         -> []

  -- Parse data out of name
  let decomposeName name = case break (== '(') name of
        (_type, mname) | [(ref, "")] <- reads mname
                         -> Just ref
        _                -> Nothing

  -- Loop through folded marks. We really only expect zero or one of them per line.
  forM_ foldedMarks $ \mark -> do
    name <- textMarkGetName mark
    iter <- textBufferGetIterAtMark coreBuffer mark

    -- Look up core
    SourceViewState{dbgMap} <- readIORef stateRef
    case name >>= decomposeName of
      Just (unit, cname, cons) | Just entry <- lookupCore dbgMap unit cname cons
        -> do -- Replace mark
              let Just core = fmap dbgCoreCode $ dbgDCore entry
              textBufferDeleteMark coreBuffer mark
              writeOpenMarkStart sview iter unit (cname, cons)
              -- Insert text
              printCore sview iter unit ltags core
              writeOpenMarkEnd sview iter unit (cname, cons)
              updateStructPixbufMap sview
              -- Update state
              modifyIORef stateRef $ \s -> s {
                openEntries = entry : openEntries s
                }
              return ()
      _ -> return ()

  -- Loop through open marks. Same as above.
  forM_ openMarks $ \mark -> do
    name <- textMarkGetName mark
    iter <- textBufferGetIterAtMark coreBuffer mark

    -- Find end mark
    case name >>= decomposeName of
      Just (unit, cname, cons) | Just entry <- lookupCore dbgMap unit cname cons
                                 -> do
        m_markEnd <- textBufferGetMark coreBuffer (markNameEnd unit (cname, cons))
        case m_markEnd of
          Nothing -> return ()
          Just markEnd -> do
            -- Get iterator for end
            iterEnd <- textBufferGetIterAtMark coreBuffer markEnd
            -- Get text, decide what prefix to keep: Indention at minimum
            slice <- textIterGetSlice iter iterEnd
            let pre = case span isSpace slice of
                  (ind, _)
                      -> length ind
            textIterForwardChars iter pre
            -- Remove old marks, insert closed mark
            textBufferDeleteMark coreBuffer mark
            textBufferDeleteMark coreBuffer markEnd
            -- Find out how many lines we span
            lineEnd <- textIterGetLine iterEnd
            -- Remove marks and text
            deleteCore coreBuffer iter iterEnd
            writeFoldedMark sview iter unit (cname, cons)
            -- Remove line annotations
            modifyIORef stateRef $ \s ->
              s { lineTags = let (pr, re) = splitAt line (lineTags s)
                             in pr ++ drop (lineEnd - line) re
                , openEntries = filter (/=entry) $ openEntries s
                }
      _ -> return ()

  -- Same again, this time for up marks
  forM_ upMarks $ \mark -> do
    name <- textMarkGetName mark
    iter <- textBufferGetIterAtMark coreBuffer mark

    -- Find end mark
    case name >>= decomposeName of
      Just (unit, cname, cons) | Just entry <- lookupCore dbgMap unit cname cons -> do
        m_markEnd <- textBufferGetMark coreBuffer (markNameEnd unit (cname, cons))
        case m_markEnd of
          Nothing -> return ()
          Just markEnd -> do
            -- Get iterator for end
            iterEnd <- textBufferGetIterAtMark coreBuffer markEnd
            -- Find out how many lines we span
            lineEnd <- textIterGetLine iterEnd
            -- Remove old text
            textBufferDeleteMark coreBuffer mark
            textBufferDeleteMark coreBuffer markEnd
            deleteCore coreBuffer iter iterEnd
            -- Remove line annotations
            modifyIORef stateRef $ \s ->
              s { lineTags = let (pr, re) = splitAt line (lineTags s)
                             in pr ++ drop (lineEnd - line) re
                }
            -- Write new parent core
            let Just core = fmap dbgCoreCode $ dbgDCore entry
                ltags = [tagByCore tags entry]
            printTopLevel sview iter unit (Just entry) ltags core
            updateStructPixbufMap sview
      _other -> return ()

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

------------------------------------------------------------------------------

-- | Subsume tags for the viewed list
subsumeTags :: [Tag] -> [Tag]
subsumeTags = nubSumBy cmp plus . map subsume
   where cmp = compare `F.on` (fmap dbgId . tagDebug)
         t1 `plus` t2 =
           t1 {
             tagEntry = case () of
                _ | tagDebug t1 == tagDebug (tagEntry t1)  -> t1
                  | tagDebug t2 == tagDebug (tagEntry t2)  -> t2
                  | otherwise                              -> t2,
             tagFreq = tagFreq t1 + tagFreq t2
           }
         subsume t = t { tagDebug = fmap getSubsumationEntry $ tagDebug t }

-- | Finds debug entry
getSubsumationEntry :: DebugEntry -> DebugEntry
getSubsumationEntry entry = case dbgParent entry of
  Nothing -> entry
  Just parent -> case dbgDCore entry of
    Just (DebugEntryCore _ _ Lam{}) -> entry
    _                               -> getSubsumationEntry parent

-- | For the lack of a better name - find direct parent/child pairs
-- and merge them
subsumeTagFamilies :: [Tag] -> [Tag]
subsumeTagFamilies tags = subsumeTags $ map moveToParent tags
 where
  dbgs = mapMaybe tagDebug tags
  moveToParent t
    | Just p <- fmap getSubsumationEntry $ (>>= dbgParent) $ tagDebug t, p `elem` dbgs
      = t { tagDebug = Just p }
    | otherwise
      = t

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
subsumeSrcTags filesOnly = map mergeSources . nubSumBy cmpSpan plus
  where cmpSpan s1 s2
          | filesOnly = stagFile s1 `compare` stagFile s2
          | otherwise = (stagName s1, stagFile s1) `compare` (stagName s2, stagFile s2)
        {--
        plusSpan s1 s2 =
          let (sl, sc) = min (startLine s1, startCol s1) (startLine s2, startCol s2)
              (el, ec) = max (endLine s1, endCol s1) (endLine s2, endCol s2)
          in s1 { startLine = sl, startCol = sc, endLine = el, endCol = ec }
        --}
        plus st1 st2 = let tags' = nub (stagTags st1 ++ stagTags st2) in
          st1 { stagSources = stagSources st1 ++ stagSources st2
              , stagTags = tags'
              , stagFreq = sum $ map tagFreq tags' }
        mergeSources st@SourceTag{stagSources}
          = st { stagSources = merge $ sortBy (comparing spanStart) stagSources }
        spanStart Span{..} = (startLine, startCol)
        spanEnd Span{..} = (endLine, endCol)
        merge (s1:ss@(s2:ss2))
          | spanEnd s1 < spanStart s2
            = s1:merge ss
          | spanEnd s1 >= spanEnd s2
            = merge (s1:ss2)
          | otherwise
            = let s1' = s1 { endLine = endLine s2, endCol = endCol s2 }
              in merge (s1':ss2)
        merge ss = ss

------------------------------------------------------------------------------

clearStructPixbufMap :: SourceView -> IO ()
clearStructPixbufMap SourceView{stateRef} =
  modifyIORef stateRef $ \s -> s { structBufMap = Map.empty }

-- | Updates the map so that there is an entry (exactly) for all
-- elements of the list.
updateStructPixbufMap :: SourceView -> IO ()
updateStructPixbufMap sview@SourceView{coreBuffer,coreView,stateRef,structRenderer} = do

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
      structBufMap' = newMap1 `Map.union` newMap2

  Map.size structBufMap' `seq`
    modifyIORef stateRef (\s -> s { structBufMap = structBufMap' } )

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


updateCoreTooltip :: SourceView -> TextIter -> IO ()
updateCoreTooltip SourceView{stateRef,coreView} iter = do

  -- Clear the tooltip
  set coreView [ widgetHasTooltip := False
               ]

  -- get identifier at location
  let isIdent '_'  = True
      isIdent '\'' = True
      isIdent '$'  = True
      isIdent '#'  = True
      isIdent x    = isAlphaNum x

      searchIdentStart iter = do
        wentBack <- textIterBackwardChar iter
        if not wentBack then return () else do
          haveIdent <- fmap (fmap isIdent) $ textIterGetChar iter
          case haveIdent of
            Just True -> searchIdentStart iter
            _other    -> textIterForwardChar iter >> return ()
      searchIdentEnd iter = do
        wentFwd <- textIterForwardChar iter
        if not wentFwd then return () else do
          haveIdent <- fmap (fmap isIdent) $ textIterGetChar iter
          case haveIdent of
            Just True -> searchIdentEnd iter
            _other    -> return ()

  searchIdentStart iter
  iterCopy <- textIterCopy iter
  searchIdentEnd iterCopy

  ident <- textIterGetSlice iter iterCopy

  -- Get line tag
  SourceViewState{lineTags} <- readIORef stateRef
  line <- textIterGetLine iter
  case drop line lineTags of
    ((tag:_):_) -> do

      -- Find type
      let identBS =strToBS ident
          mtyp = findDbgElem ((getBindType identBS . dbgCoreCode =<<) . dbgDCore)
                 (tagDebug tag)
      case mtyp of
        Just typ -> do
          set coreView
            [ widgetHasTooltip := False
            , widgetTooltipText := Just $ bsToStr typ
            ]
        Nothing -> return ()

    _other -> return ()

strToBS :: String -> BS.ByteString
strToBS = BS.pack . map (fromIntegral.ord)
