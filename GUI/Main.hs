{-# LANGUAGE CPP #-}
module GUI.Main (runGUI) where

-- Imports for GTK
import qualified Graphics.UI.Gtk as Gtk
import System.Glib.GError (failOnGError)

-- Imports from Haskell library
import Text.Printf
#ifndef mingw32_HOST_OS
import System.Posix
#endif
import Control.Concurrent
import qualified Control.Concurrent.Chan as Chan
import Control.Exception
import Data.Array
import Data.Maybe

import Paths_threadscope_pmw

-- Imports for ThreadScope
import qualified GUI.MainWindow as MainWindow
import GUI.Types
import Events.HECs hiding (Event)
import Events.Debug
import GUI.Dialogs
import Events.ReadEvents
import GUI.EventsView
import GUI.SummaryView
import GUI.StartupInfoView
import GUI.Histogram
import GUI.Timeline
import GUI.TraceView
import GUI.BookmarkView
import GUI.KeyView
import GUI.SourceView
import GUI.SaveAs
import qualified GUI.ConcurrencyControl as ConcurrencyControl
import qualified GUI.ProgressView as ProgressView
import qualified GUI.GtkExtras as GtkExtras

-------------------------------------------------------------------------------

data UIEnv = UIEnv {

       mainWin       :: MainWindow.MainWindow,
       eventsView    :: EventsView,
       startupView   :: StartupInfoView,
       summaryView   :: SummaryView,
       histogramView :: HistogramView,
       timelineWin   :: TimelineView,
       traceView     :: TraceView,
       bookmarkView  :: BookmarkView,
       keyView       :: KeyView,
       sourceView    :: SourceView,

       eventQueue    :: Chan Event,
       concCtl       :: ConcurrencyControl.ConcurrencyControl
     }

data EventlogState
   = NoEventlogLoaded
   | EventlogLoaded {
       mfilename :: Maybe FilePath, --test traces have no filepath
       hecs      :: HECs,
       selection :: TimeSelection,
       cursorPos :: Int,
       currentPages :: [MainWindow.MainWindowPage] -- pages that don't need to be refreshed
     }

postEvent :: Chan Event -> Event -> IO ()
postEvent = Chan.writeChan

getEvent ::  Chan Event -> IO Event
getEvent = Chan.readChan

data Event
   = EventOpenDialog
   | EventExportDialog
   | EventLaunchWebsite
   | EventLaunchTutorial
   | EventAboutDialog
   | EventQuit

   | EventFileLoad   FilePath
   | EventTestLoad   String
   | EventFileReload
   | EventFileExport FilePath FileExportFormat

-- | EventStateClear
   | EventSetState HECs DebugMaps (Maybe FilePath) String Int Double

   | EventShowSidebar Bool
   | EventShowEvents  Bool

   | EventTimelineJumpStart
   | EventTimelineJumpEnd
   | EventTimelineJumpCursor
   | EventTimelineScrollLeft
   | EventTimelineScrollRight
   | EventTimelineZoomIn
   | EventTimelineZoomOut
   | EventTimelineZoomToFit
   | EventTimelineLabelsMode Bool
   | EventTimelineShowBW     Bool

   | EventSwitchPage MainWindow.MainWindowPage

   | EventCursorChangedIndex     Int
   | EventCursorChangedSelection TimeSelection

   | EventTracesChanged [Trace]

   | EventBookmarkAdd
   | EventBookmarkRemove Int
   | EventBookmarkEdit   Int String

   | EventUserError String SomeException
                    -- can add more specific ones if necessary

constructUI :: Options -> IO UIEnv
constructUI opts = failOnGError $ do

  builder <- Gtk.builderNew
  Gtk.builderAddFromFile builder =<< getDataFileName "threadscope.ui"

  eventQueue <- Chan.newChan
  let post = postEvent eventQueue

  mainWin <- MainWindow.mainWindowNew builder MainWindow.MainWindowActions {
    mainWinOpen          = post EventOpenDialog,
    mainWinExport        = post EventExportDialog,
    mainWinQuit          = post EventQuit,
    mainWinViewSidebar   = post . EventShowSidebar,
    mainWinViewEvents    = post . EventShowEvents,
    mainWinViewReload    = post EventFileReload,
    mainWinWebsite       = post EventLaunchWebsite,
    mainWinTutorial      = post EventLaunchTutorial,
    mainWinAbout         = post EventAboutDialog,
    mainWinJumpStart     = post EventTimelineJumpStart,
    mainWinJumpEnd       = post EventTimelineJumpEnd,
    mainWinJumpCursor    = post EventTimelineJumpCursor,
    mainWinScrollLeft    = post EventTimelineScrollLeft,
    mainWinScrollRight   = post EventTimelineScrollRight,
    mainWinJumpZoomIn    = post EventTimelineZoomIn,
    mainWinJumpZoomOut   = post EventTimelineZoomOut,
    mainWinJumpZoomFit   = post EventTimelineZoomToFit,
    mainWinDisplayLabels = post . EventTimelineLabelsMode,
    mainWinViewBW        = post . EventTimelineShowBW,
    mainWinSwitchPage    = post . EventSwitchPage
  }

  timelineWin <- timelineViewNew builder TimelineViewActions {
    timelineViewSelectionChanged = post . EventCursorChangedSelection
  }

  eventsView <- eventsViewNew builder EventsViewActions {
    eventsViewCursorChanged = post . EventCursorChangedIndex
  }

  startupView <- startupInfoViewNew builder
  summaryView <- summaryViewNew builder

  histogramView <- histogramViewNew builder

  traceView <- traceViewNew builder TraceViewActions {
    traceViewTracesChanged = post . EventTracesChanged
  }

  sourceView <- sourceViewNew builder opts SourceViewActions {}

  bookmarkView <- bookmarkViewNew builder BookmarkViewActions {
    bookmarkViewAddBookmark    = post EventBookmarkAdd,
    bookmarkViewRemoveBookmark = post . EventBookmarkRemove,
    bookmarkViewGotoBookmark   = \ts -> do
      post (EventCursorChangedSelection (PointSelection ts))
      post EventTimelineJumpCursor,
    bookmarkViewEditLabel      = \n v -> post (EventBookmarkEdit n v)
  }

  keyView <- keyViewNew builder

  concCtl <- ConcurrencyControl.start

  return UIEnv{..}

-------------------------------------------------------------------------------

data LoopDone = LoopDone

eventLoop :: UIEnv -> EventlogState -> IO ()
eventLoop uienv@UIEnv{..} eventlogState = do

    event <- getEvent eventQueue
    next  <- dispatch event eventlogState
#if __GLASGOW_HASKELL__ <= 612
               -- workaround for a wierd exception handling bug in ghc-6.12
               `Control.Exception.catch` \e -> throwIO (e :: SomeException)
#endif
    case next of
      Left  LoopDone       -> return ()
      Right eventlogState' -> eventLoop uienv eventlogState'

  where
    dispatch :: Event -> EventlogState -> IO (Either LoopDone EventlogState)

    dispatch EventQuit _ = return (Left LoopDone)

    dispatch EventOpenDialog _ = do
      openFileDialog mainWin $ \filename ->
        post (EventFileLoad filename)
      continue

    dispatch (EventFileLoad filename) _ = do
      async "loading the eventlog" $
        loadEvents (Just filename) (registerEventsFromFile filename)
      --TODO: set state to be empty during loading
      continue

    dispatch (EventTestLoad testname) _ = do
      async "loading the test eventlog" $
        loadEvents Nothing (registerEventsFromTrace testname)
      --TODO: set state to be empty during loading
      continue

    dispatch EventFileReload EventlogLoaded{mfilename = Just filename} = do
      async "reloading the eventlog" $
        loadEvents (Just filename) (registerEventsFromFile filename)
      --TODO: set state to be empty during loading
      continue

    dispatch EventFileReload EventlogLoaded{mfilename = Nothing} =
      continue

--    dispatch EventClearState _

    dispatch (EventSetState hecs dbgMap mfilename name nevents timespan) _ = do

      MainWindow.setFileLoaded mainWin (Just name)
      MainWindow.setStatusMessage mainWin $
        printf "%s (%d events, %.3fs)" name nevents timespan

      let mevents = Just $ hecEventArray hecs
      eventsViewSetEvents eventsView mevents
      sourceViewSetEvents sourceView mfilename (Just (hecEventArray hecs, dbgMap))
      startupInfoViewSetEvents startupView mevents
      summaryViewSetEvents summaryView mevents
      histogramViewSetHECs histogramView (Just hecs)
      traceViewSetHECs traceView hecs
      traces' <- traceViewGetTraces traceView
      timelineWindowSetHECs timelineWin (Just hecs)
      timelineWindowSetTraces timelineWin traces'

      -- TODO: disabled for now, until it's configurable (see the TODO file)
      -- We set user 'traceEvent' messages as initial bookmarks.
      -- This is somewhat of an experiment. If users use lots of trace events
      -- then it will not be appropriate and we'll want a separate 'traceMark'.
      --let usrMsgs = extractUserMessages hecs
      --sequence_ [ bookmarkViewAdd bookmarkView ts label
      --          | (ts, label) <- usrMsgs ]
      -- timelineWindowSetBookmarks timelineWin (map fst usrMsgs)
      bookmarkViewClear bookmarkView
      timelineWindowSetBookmarks timelineWin []

      if nevents == 0
        then continueWith NoEventlogLoaded
        else continueWith EventlogLoaded
          { mfilename = mfilename
          , hecs      = hecs
          , selection = PointSelection 0
          , cursorPos = 0
          , currentPages = []
          }

    dispatch EventExportDialog
             EventlogLoaded {mfilename} = do
      exportFileDialog mainWin (fromMaybe "" mfilename) $ \filename' format ->
        post (EventFileExport filename' format)
      continue

    dispatch (EventFileExport filename format)
             EventlogLoaded {hecs} = do
      viewParams <- timelineGetViewParameters timelineWin
      let viewParams' = viewParams {
                          detail     = 1,
                          bwMode     = False,
                          labelsMode = False
                        }
      let yScaleArea = timelineGetYScaleArea timelineWin
      case format of
        FormatPDF ->
          saveAsPDF filename hecs viewParams' yScaleArea
        FormatPNG ->
          saveAsPNG filename hecs viewParams' yScaleArea
      continue

    dispatch EventLaunchWebsite _ = do
      GtkExtras.launchProgramForURI "http://www.haskell.org/haskellwiki/ThreadScope"
      continue

    dispatch EventLaunchTutorial _ = do
      GtkExtras.launchProgramForURI "http://www.haskell.org/haskellwiki/ThreadScope_Tour"
      continue

    dispatch EventAboutDialog _ = do
      aboutDialog mainWin
      continue

    dispatch (EventShowSidebar visible) _ = do
      MainWindow.sidebarSetVisibility mainWin visible
      continue

    dispatch (EventShowEvents visible) _ = do
      MainWindow.eventsSetVisibility mainWin visible
      continue

    dispatch EventTimelineJumpStart _ = do
      timelineScrollToBeginning timelineWin
      eventsViewScrollToLine eventsView 0
      continue

    dispatch EventTimelineJumpEnd EventlogLoaded{hecs} = do
      timelineScrollToEnd timelineWin
      let (_,end) = bounds (hecEventArray hecs)
      eventsViewScrollToLine eventsView end
      continue

    dispatch EventTimelineJumpCursor EventlogLoaded{cursorPos} = do
      timelineCentreOnCursor timelineWin --TODO: pass selection here
      eventsViewScrollToLine eventsView cursorPos
      continue

    dispatch EventTimelineScrollLeft  _ = do
      timelineScrollLeft  timelineWin
      continue

    dispatch EventTimelineScrollRight _ = do
      timelineScrollRight timelineWin
      continue
    dispatch EventTimelineZoomIn      _ = do
      timelineZoomIn    timelineWin
      continue
    dispatch EventTimelineZoomOut     _ = do
      timelineZoomOut   timelineWin
      continue
    dispatch EventTimelineZoomToFit   _ = do
      timelineZoomToFit timelineWin
      continue

    dispatch (EventTimelineLabelsMode labelsMode) _ = do
      timelineSetLabelsMode timelineWin labelsMode
      continue

    dispatch (EventTimelineShowBW showBW) _ = do
      timelineSetBWMode timelineWin showBW
      continue

    dispatch (EventCursorChangedIndex cursorPos') EventlogLoaded{hecs} = do
      let cursorTs'  = eventIndexToTimestamp hecs cursorPos'
      refreshView eventlogState {
        selection = PointSelection cursorTs',
        cursorPos = cursorPos'
        }

    dispatch (EventCursorChangedSelection selection'@(PointSelection cursorTs'))
             eventLogState@EventlogLoaded{hecs} = do
      let cursorPos' = timestampToEventIndex (hecEventArray hecs) cursorTs'
      refreshView eventLogState {
        selection = selection',
        cursorPos = cursorPos'
        }

    dispatch (EventCursorChangedSelection selection'@(RangeSelection start _))
             eventLogState@EventlogLoaded{hecs} = do
      let cursorPos' = timestampToEventIndex (hecEventArray hecs) start
      refreshView eventLogState {
        selection = selection',
        cursorPos = cursorPos'
        }

    dispatch (EventSwitchPage page) eventlogState =
      refreshPage page eventlogState

    dispatch (EventTracesChanged traces) _ = do
      timelineWindowSetTraces timelineWin traces
      continue

    dispatch EventBookmarkAdd EventlogLoaded{selection} = do
      case selection of
        PointSelection a   -> bookmarkViewAdd bookmarkView a ""
        RangeSelection a b -> do bookmarkViewAdd bookmarkView a ""
                                 bookmarkViewAdd bookmarkView b ""
      --TODO: should have a way to add/set a single bookmark for the timeline
      -- rather than this hack where we ask the bookmark view for the whole lot.
      ts <- bookmarkViewGet bookmarkView
      timelineWindowSetBookmarks timelineWin (map fst ts)
      continue

    dispatch (EventBookmarkRemove n) _ = do
      bookmarkViewRemove bookmarkView n
      --TODO: should have a way to add/set a single bookmark for the timeline
      -- rather than this hack where we ask the bookmark view for the whole lot.
      ts <- bookmarkViewGet bookmarkView
      timelineWindowSetBookmarks timelineWin (map fst ts)
      continue

    dispatch (EventBookmarkEdit n v) _ = do
      bookmarkViewSetLabel bookmarkView n v
      continue

    dispatch (EventUserError doing exception) _ = do
      let headline    = "There was a problem " ++ doing ++ "."
          explanation = show exception
      errorMessageDialog mainWin headline explanation
      continue

    dispatch _ NoEventlogLoaded = continue

    loadEvents mfilename registerEvents = do
      ConcurrencyControl.fullSpeed concCtl $
        ProgressView.withProgress mainWin $ \progress -> do
          (hecs, name, nevents, timespan) <- registerEvents progress
          (dbgMap, events') <- buildDebugMaps (hecEventArray hecs) (ProgressView.setText progress)
          -- This is a desperate hack to avoid the "segfault on reload" bug
          -- http://trac.haskell.org/ThreadScope/ticket/1
          -- It should be enough to let other threads finish and so avoid
          -- re-entering gtk C code (see ticket for the dirty details)
          threadDelay 100000 -- 1/10th of a second
          post (EventSetState hecs dbgMap mfilename name nevents timespan)
      return ()

    async doing action =
      forkIO (action `Control.Exception.catch` \e -> post (EventUserError doing e))

    refreshView eventlogState@EventlogLoaded{..} = do
      timelineSetSelection timelineWin selection

      -- Refresh active page, invalidate all others
      activePage <- MainWindow.getCurrentPage mainWin
      mapM_ invalidatePage (filter (/= activePage) currentPages)
      refreshPage activePage eventlogState{ currentPages = [] }
    refreshView eventlogState = continueWith eventlogState

    invalidatePage :: MainWindow.MainWindowPage -> IO ()
    invalidatePage MainWindow.PageSource = sourceViewClear sourceView
    invalidatePage _                     = return ()

    refreshPage page eventlogState@EventlogLoaded{..}
      | page `elem` currentPages  = continueWith eventlogState
      | otherwise                 = do
          -- Interval needs adjustment?
          selection' <- case page of
            MainWindow.PageSource -> sourceViewAdjustSelection sourceView selection
            _                     -> return Nothing
          case selection' of
            Just sel' -> do post (EventCursorChangedSelection sel')
                            continueWith eventlogState
            Nothing   -> do
              let mrange = case selection of
                    PointSelection _     -> Nothing
                    RangeSelection _ end -> Just (cursorPos,
                                                  timestampToEventIndex (hecEventArray hecs) end)
                  interval = case selection of
                    PointSelection _         -> Nothing
                    RangeSelection start end -> Just (start, end)
              case page of
                MainWindow.PageSummary -> summaryViewSetInterval summaryView interval
                MainWindow.PageStartup -> return ()
                MainWindow.PageSparks  -> histogramViewSetInterval histogramView interval
                MainWindow.PageEvents  -> eventsViewSetCursor eventsView cursorPos mrange
                MainWindow.PageSource  -> sourceViewSetSelection sourceView selection
              continueWith eventlogState {
                currentPages = page:currentPages
                }
    refreshPage _  eventlogState = continueWith eventlogState

    post = postEvent eventQueue
    continue = continueWith eventlogState
    continueWith = return . Right

-------------------------------------------------------------------------------

runGUI :: Maybe (Either FilePath String) -> Options -> IO ()
runGUI initialTrace opts = do
  Gtk.initGUI

  uiEnv <- constructUI opts

  let post = postEvent (eventQueue uiEnv)

  case initialTrace of
   Nothing                -> return ()
   Just (Left  filename)  -> post (EventFileLoad filename)
   Just (Right traceName) -> post (EventTestLoad traceName)

  doneVar <- newEmptyMVar

  forkIO $ do
    res <- try $ eventLoop uiEnv NoEventlogLoaded
    Gtk.mainQuit
    putMVar doneVar (res :: Either SomeException ())

#ifndef mingw32_HOST_OS
  installHandler sigINT (Catch $ post EventQuit) Nothing
#endif

  -- Enter Gtk+ main event loop.
  Gtk.mainGUI

  -- Wait for child event loop to terminate
  -- This lets us wait for any exceptions.
  either throwIO return =<< takeMVar doneVar
