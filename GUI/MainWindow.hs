module GUI.MainWindow (
    MainWindow,
    mainWindowNew,
    MainWindowActions(..),
    MainWindowPage(..),

    setFileLoaded,
    setStatusMessage,
    sidebarSetVisibility,
    eventsSetVisibility,
    getCurrentPage,

  ) where

import Paths_threadscope_pmw

-- Imports for GTK
import Graphics.UI.Gtk as Gtk
import qualified System.Glib.GObject as Glib


-------------------------------------------------------------------------------

data MainWindow = MainWindow {
       mainWindow         :: Window,

       sidebarBox         :: Widget,
       eventsBox          :: Notebook,

       statusBar          :: Statusbar,
       statusBarCxt       :: ContextId
     }

instance Glib.GObjectClass  MainWindow where
  toGObject = toGObject . mainWindow
  unsafeCastGObject = error "cannot downcast to MainView type"

instance Gtk.ObjectClass    MainWindow
instance Gtk.WidgetClass    MainWindow
instance Gtk.ContainerClass MainWindow
instance Gtk.BinClass       MainWindow
instance Gtk.WindowClass    MainWindow

data MainWindowActions = MainWindowActions {

       -- Menu actions
       mainWinOpen          :: IO (),
       mainWinExport        :: IO (),
       mainWinQuit          :: IO (),
       mainWinViewSidebar   :: Bool -> IO (),
       mainWinViewEvents    :: Bool -> IO (),
       mainWinViewBW        :: Bool -> IO (),
       mainWinViewReload    :: IO (),
       mainWinWebsite       :: IO (),
       mainWinTutorial      :: IO (),
       mainWinAbout         :: IO (),

       -- Toolbar actions
       mainWinJumpStart     :: IO (),
       mainWinJumpEnd       :: IO (),
       mainWinJumpCursor    :: IO (),
       mainWinJumpZoomIn    :: IO (),
       mainWinJumpZoomOut   :: IO (),
       mainWinJumpZoomFit   :: IO (),
       mainWinScrollLeft    :: IO (),
       mainWinScrollRight   :: IO (),
       mainWinDisplayLabels :: Bool -> IO (),

       -- Pane actions
       mainWinSwitchPage    :: MainWindowPage -> IO ()
     }

data MainWindowPage = PageSummary
                    | PageStartup
                    | PageSparks
                    | PageEvents
                    | PageSource
                    deriving (Eq)

-------------------------------------------------------------------------------

setFileLoaded :: MainWindow -> Maybe FilePath -> IO ()
setFileLoaded mainWin Nothing =
  set (mainWindow mainWin) [
      windowTitle := "ThreadScope"
    ]
setFileLoaded mainWin (Just file) =
  set (mainWindow mainWin) [
      windowTitle := file ++ " - ThreadScope"
    ]

setStatusMessage :: MainWindow -> String -> IO ()
setStatusMessage mainWin msg = do
  statusbarPop  (statusBar mainWin) (statusBarCxt mainWin)
  statusbarPush (statusBar mainWin) (statusBarCxt mainWin) (' ':msg)
  return ()

sidebarSetVisibility :: MainWindow -> Bool -> IO ()
sidebarSetVisibility mainWin visible =
  set (sidebarBox mainWin) [ widgetVisible := visible ]

eventsSetVisibility :: MainWindow -> Bool -> IO ()
eventsSetVisibility mainWin visible =
  set (eventsBox mainWin) [ widgetVisible := visible ]

-------------------------------------------------------------------------------

mainWindowNew :: Builder -> MainWindowActions -> IO MainWindow
mainWindowNew builder actions = do

  let getWidget cast name = builderGetObject builder cast name


  mainWindow         <- getWidget castToWindow "main_window"
  statusBar          <- getWidget castToStatusbar "statusbar"

  sidebarBox         <- getWidget castToWidget "sidebar"
  eventsBox          <- getWidget castToNotebook "eventsbox"

  bwToggle           <- getWidget castToCheckMenuItem "black_and_white"
  labModeToggle      <- getWidget castToCheckMenuItem "view_labels_mode"
  sidebarToggle      <- getWidget castToCheckMenuItem "view_sidebar"
  eventsToggle       <- getWidget castToCheckMenuItem "view_events"
  openMenuItem       <- getWidget castToMenuItem "openMenuItem"
  exportMenuItem     <- getWidget castToMenuItem "exportMenuItem"
  reloadMenuItem     <- getWidget castToMenuItem "view_reload"
  quitMenuItem       <- getWidget castToMenuItem "quitMenuItem"
  websiteMenuItem    <- getWidget castToMenuItem "websiteMenuItem"
  tutorialMenuItem   <- getWidget castToMenuItem "tutorialMenuItem"
  aboutMenuItem      <- getWidget castToMenuItem "aboutMenuItem"

  firstMenuItem      <- getWidget castToMenuItem "move_first"
  centreMenuItem     <- getWidget castToMenuItem "move_centre"
  lastMenuItem       <- getWidget castToMenuItem "move_last"

  zoomInMenuItem     <- getWidget castToMenuItem "move_zoomin"
  zoomOutMenuItem    <- getWidget castToMenuItem "move_zoomout"
  zoomFitMenuItem    <- getWidget castToMenuItem "move_zoomfit"

  openButton         <- getWidget castToToolButton "cpus_open"

  firstButton        <- getWidget castToToolButton "cpus_first"
  centreButton       <- getWidget castToToolButton "cpus_centre"
  lastButton         <- getWidget castToToolButton "cpus_last"

  zoomInButton       <- getWidget castToToolButton "cpus_zoomin"
  zoomOutButton      <- getWidget castToToolButton "cpus_zoomout"
  zoomFitButton      <- getWidget castToToolButton "cpus_zoomfit"

  --TODO: this is currently not used, but it'be nice if it were!
  eventsTextEntry    <- getWidget castToEntry      "events_entry"

  ------------------------------------------------------------------------
  -- Show everything
  widgetShowAll mainWindow

  widgetHide eventsTextEntry  -- for now we hide it, see above.

  ------------------------------------------------------------------------

  logoPath <- getDataFileName "threadscope.png"
  windowSetIconFromFile mainWindow logoPath

  ------------------------------------------------------------------------
  -- Status bar functionality

  statusBarCxt <- statusbarGetContextId statusBar "file"
  statusbarPush statusBar statusBarCxt "No eventlog loaded."

  ------------------------------------------------------------------------
  -- Bind all the events

  -- Menus
  on openMenuItem      menuItemActivate $ mainWinOpen actions
  on exportMenuItem    menuItemActivate $ mainWinExport actions

  on quitMenuItem menuItemActivate $ mainWinQuit actions
  on mainWindow   objectDestroy    $ mainWinQuit actions

  on sidebarToggle  checkMenuItemToggled $ checkMenuItemGetActive sidebarToggle
                                       >>= mainWinViewSidebar   actions
  on eventsToggle   checkMenuItemToggled $ checkMenuItemGetActive eventsToggle
                                       >>= mainWinViewEvents    actions
  on bwToggle       checkMenuItemToggled $ checkMenuItemGetActive bwToggle
                                       >>= mainWinViewBW        actions
  on labModeToggle  checkMenuItemToggled $ checkMenuItemGetActive labModeToggle
                                       >>= mainWinDisplayLabels actions
  on reloadMenuItem menuItemActivate     $ mainWinViewReload actions

  on websiteMenuItem  menuItemActivate    $ mainWinWebsite actions
  on tutorialMenuItem menuItemActivate    $ mainWinTutorial actions
  on aboutMenuItem    menuItemActivate    $ mainWinAbout actions

  on firstMenuItem   menuItemActivate     $ mainWinJumpStart  actions
  on centreMenuItem  menuItemActivate     $ mainWinJumpCursor actions
  on lastMenuItem    menuItemActivate     $ mainWinJumpEnd    actions

  on zoomInMenuItem  menuItemActivate     $ mainWinJumpZoomIn  actions
  on zoomOutMenuItem menuItemActivate     $ mainWinJumpZoomOut actions
  on zoomFitMenuItem menuItemActivate     $ mainWinJumpZoomFit actions

  -- Toolbar
  onToolButtonClicked openButton $ mainWinOpen       actions

  onToolButtonClicked firstButton  $ mainWinJumpStart  actions
  onToolButtonClicked centreButton $ mainWinJumpCursor actions
  onToolButtonClicked lastButton   $ mainWinJumpEnd    actions

  onToolButtonClicked zoomInButton  $ mainWinJumpZoomIn  actions
  onToolButtonClicked zoomOutButton $ mainWinJumpZoomOut actions
  onToolButtonClicked zoomFitButton $ mainWinJumpZoomFit actions

  -- Pane
  on eventsBox switchPage $ mainWinSwitchPage actions . pageNoToPage

  return MainWindow {..}

getCurrentPage :: MainWindow -> IO MainWindowPage
getCurrentPage MainWindow{eventsBox}
  = fmap pageNoToPage $ notebookGetCurrentPage eventsBox

-- Note: This isn't stable against page reordering. Might be better
-- to use notebookGetNthPageSource in future and check it against
-- the Widget names from the glade file.
pageNoToPage :: Int -> MainWindowPage
pageNoToPage 0 = PageSummary
pageNoToPage 1 = PageStartup
pageNoToPage 2 = PageSparks
pageNoToPage 3 = PageEvents
pageNoToPage 4 = PageSource
pageNoToPage _ = error "pageNoToPage: Unknown page number!"
