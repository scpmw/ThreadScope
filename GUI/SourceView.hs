
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

import Trace.Hpc.Mix (Mix(..), readMix)
import Trace.Hpc.Cix (CixTree(..), CixInfo(..), CixType(..), readCix)
import Trace.Hpc.Util (insideHpcPos, fromHpcPos)

import Text.Objdump  (ObjRangeType(..), ObjRange(..), readObjRanges)

import Data.Array
import Data.IORef
import Data.Maybe (mapMaybe, catMaybes, fromJust)
import Data.List
import Data.Word (Word32)
import qualified Data.Function as F
import qualified Data.IntMap as IM
import Data.Char (isDigit, ord)

import System.FilePath
import System.Directory (doesFileExist,getCurrentDirectory,canonicalizePath)

import Control.Monad (forM_, forM, when)
import Control.Applicative ((<$>))
import Control.Arrow (first, second)

import Numeric (showHex)

import Text.Printf

-------------------------------------------------------------------------------

data SourceView = SourceView {
  stateRef     :: !(IORef SourceViewState),
  sourceView   :: !TextView,
  sourceBuffer :: !TextBuffer,
  tagsStore    :: !(ListStore Tag),
  tagsTreeView :: !TreeView
  }

type EventsArray = Array Int CapEvent
type FileMap = [(String, FilePath)]
type UnitMap = [(FilePath, String)]
type CixMap = [(Int, [Int])] -- TODO: Right now this is completely module-oblivious!
type RangeMap = IM.IntMap ObjRange
type CoreMap = [(Int, String)]

data Tag = Tag {
  -- | Module this tag belongs to. If @Nothing@, the tag could not be
  -- assigned to a module (e.g. a global symbol)
  tagModule :: Maybe String, 
  -- | Name of the tag. If @Nothing@, the tag does not have a name, as
  -- will be the case for most Haskell expressions
  tagName :: Maybe String,
  -- | Tick number of the tag. This is only set if there's a Haskell
  -- expression the tag could be mapped to
  tagTick :: Maybe Word32,
  -- | Approximate frequency of the tag getting hit
  tagFreq :: !Double
  } deriving (Show)

instance Eq Tag where
  (Tag m1 n1 t1 _) == (Tag m2 n2 t2 _)  = m1 == m2 && (n1 == n2 || t1 == t2)
  
data SourceViewState 
  = StateEmpty
  | StateLoaded {
    eventsArr  :: EventsArray,
    fileMap    :: FileMap,                  -- ^ Map from all loaded modules to their actual file names
    unitMap    :: UnitMap,                  -- ^ Maps compilation unit names (from ranges) to module names
    mixData    :: [(String, Mix)],          -- ^ Mix for all loaded modules
    cixData    :: [(String, CixTree)],      -- ^ Cix for all loaded modules
    cixMaps    :: [(String, CixMap)],
    coreMaps   :: [(String, CoreMap)],
    rangeMap   :: RangeMap,
    tags       :: [Tag],
    selection  :: Maybe Tag,
    currentMod :: Maybe String
  }
    
-- | TODO after prototype crunch. This would make a lot more sense...
{--
data SourceModule = 
  { mixData :: Mix
  , cixData :: CixTree
  , cixMap :: CixMap
  }
--}
   
data SourceViewActions = SourceViewActions {
  --sourceViewRedraw :: IO ()
  }

sourceViewNew :: Builder -> SourceViewActions -> IO SourceView
sourceViewNew builder SourceViewActions{..} = do
  stateRef <- newIORef StateEmpty
  
  let getWidget cast = builderGetObject builder cast
  sourceView   <- getWidget castToTextView "source"
  sourceBuffer <- textViewGetBuffer sourceView
  
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
  (tagTickCol, tagTickRender) <- mkColumn "Tk"
  (tagNameCol, tagNameRender) <- mkColumn "Name"
  
  treeViewSetModel tagsTreeView tagsStore
  
  cellLayoutSetAttributes tagFreqCol tagFreqRender tagsStore $ \Tag{..} ->
    [ cellText := printf "%02.1f" (tagFreq * 100) ]
  cellLayoutSetAttributes tagTickCol tagTickRender tagsStore $ \Tag{..} ->
    [ cellText := maybe "" show tagTick ]
  cellLayoutSetAttributes tagNameCol tagNameRender tagsStore $ \Tag{..} ->
    [ cellText := intercalate " / " (maybe [] (\m->[m]) tagModule ++ 
                                     maybe [] (\m->[m]) tagName)]
  
  let srcView    = SourceView {..}
      
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
      
      -- Load ranges from the executable
      let exeName = dropExtension file
      putStr $ "Loading ranges from " ++ exeName ++ "... "
      ipRanges <- readObjRanges exeName
      putStrLn $ show (length ipRanges) ++ " ranges found"
      
      -- Find source files
      putStr $ "Searching sources... "
      (fileMap, unitMap) <-
        findSourceFiles searchDirs $ nub $
          [ raUnit | ObjRange {raUnit} <- ipRanges, takeExtension raUnit == ".hs"]
      putStrLn $ show (length fileMap) ++ " files found: " ++ show fileMap
      
      -- Build range map      
      let rangeMap = buildRangeMap ipRanges
      --mapM_ print $ IM.toAscList rangeMap
      
      -- Find mappings for all source files
      putStr $ "Loading Mix/Cix... "
      (mixData, cixData) <- loadMixes searchDirs fileMap
      putStrLn $ "got " ++ show (length mixData) ++ " Mix and " 
        ++ show (length cixData) ++ " Cix"
      
      let (cixMaps, coreMaps) = unzip $ map (uncurry f) cixData
          f m cix = let mix = fromJust (lookup m mixData)
                        (cxm, cdm) = mkCixMap cix mix
                    in ((m, cxm), (m, cdm))
      putStrLn $ "I have " ++ show (length $ nub $ concatMap snd $ concatMap snd cixMaps) 
        ++ " source code locations and " ++ show (length $ concatMap snd cixMaps) 
        ++ " instrumentation points for annotation"
      
      let tags = tagsFromLocalTicks 0 eventsArr ++ tagsFromLocalIPs 0 eventsArr rangeMap unitMap
          selection = Nothing
          currentMod = Nothing
      
      return StateLoaded {..}
    
    _other -> return StateEmpty
      
------------------------------------------------------------------------------
    
searchGen :: (FilePath -> IO Bool) -> [FilePath] -> FilePath -> IO (Maybe FilePath)
searchGen _    []         _    = return Nothing
searchGen test (dir:dirs) name = do
  full_name <- canonicalizePath (combine dir name)
  ex <- test full_name
  if ex 
    then return (Just full_name)
    else searchGen test dirs name

searchFile{-, searchDirectory-} :: [FilePath] -> FilePath -> IO (Maybe FilePath)
searchFile = searchGen doesFileExist
--searchDirectory = searchGen doesDirectoryExist

findSourceFiles :: [FilePath] -> [FilePath] -> IO (FileMap, UnitMap)
findSourceFiles searchDirs sources = unzip . catMaybes <$> mapM findSource sources
 where 
   findSource source = do
     -- Look for the source file
     m_file <- searchFile searchDirs source
     case m_file of
       Nothing -> return Nothing -- There simply must be a way to get around this.
       Just file -> do
         m_name <- getModuleName file
         case m_name of
           Nothing -> return Nothing
           Just name -> return $ Just ((name, file), (source, name))

-- | Extracts the module name from a haskell source file. *Very*
-- naive, should be improved.
getModuleName :: FilePath -> IO (Maybe String)
getModuleName file = do
  let searchName ("module":name:_) = Just name
      searchName (_:xs)            = searchName xs
      searchName []                = Nothing
  searchName . words <$> readFile file

------------------------------------------------------------------------------

{-

-- | Extracts @HpcModule@ events from the event stream. Works only
-- sporadically right now, investigation pending.
modulesFromEvent :: EventsArray -> [String]
modulesFromEvent eventsArr =
  -- Find all modules mentioned in the event log (at least the first second of it)
  let getMod CapEvent { ce_event = Event { spec = HpcModule {modName} } }
        = Just modName
      getMod _
        = Nothing
      windowLength = 100 * 1000 * 1000 -- Look in the first 100ms
  in mapMaybe (getMod . snd) $ 
     takeWhile ((< windowLength) . fst) $ 
     assocs eventsArr

-}

------------------------------------------------------------------------------
  
loadMixes :: [FilePath] -> [(String, FilePath)] -> IO ([(String, Mix)], [(String, CixTree)])
loadMixes searchDirs fileMap = do
  
  let mods = map fst fileMap
      hpcDirs = map (</> ".hpc") searchDirs
        
  -- Load Mix & Cix information
  mixes <- forM mods (\mod -> (,) mod <$> readMix hpcDirs (Left mod))
  cixes <- forM mods (\mod -> (,) mod <$> readCix hpcDirs (Left mod))
  return (mixes, cixes)

------------------------------------------------------------------------------

sourceViewSetCursor :: SourceView -> Int -> IO ()
sourceViewSetCursor view@SourceView {..} n = do
  state <- readIORef stateRef
  case state of
    StateLoaded{..} -> do
      
      -- Load tags for new position
      let n' = clampBounds (bounds eventsArr) n
          tags' = tagsFromLocalTicks n' eventsArr ++ tagsFromLocalIPs n' eventsArr rangeMap unitMap
          
      -- Update selection, if possible
      let selection' = selection >>= (\t -> find (== t) tags')
      
      -- Set new state
      writeIORef stateRef state{ tags=tags', selection=selection' }
      
      -- Update views
      updateTagsView tagsStore tags'
      setTagSelection view selection'
      showModule view "Main"
    
    _ -> clearAll view

clearAll :: SourceView -> IO ()
clearAll SourceView{..} = do
  listStoreClear tagsStore
  textBufferSetText sourceBuffer ""

updateTagsView :: ListStore Tag -> [Tag] -> IO ()
updateTagsView tagsStore tags = do
  listStoreClear tagsStore
  let sorted = sortBy (flip (compare `F.on` tagFreq)) tags  
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
      showModule view "Main"
      updateTextTags view
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

splitGlobalTick :: Word32 -> (String, Word32)
splitGlobalTick n = ("Main", n) -- TODO!

findLocalTicks :: Int -> EventsArray -> [(String, Word32, Double)]
findLocalTicks startIx eventsArr =
  
  -- Find ticks around start point
  let winMid        = time $ ce_event $ eventsArr ! startIx
      samples       = findLocalTickSamples startIx eventsArr
      
      -- Weight them by distance to start point
      weighted      = map (uncurry $ weightTicks winMid) samples
      
      -- Group together same tick IDs
      sorted        = sortBy (compare `F.on` fst) $ concat weighted
      grouped       = groupBy ((==) `F.on` fst) sorted
      
      -- Sum up weighted frequencies by tick IDs
      summed        = map (\ts -> (fst $ head ts, sum $ map snd ts)) grouped
      grandSum      = sum $ map snd summed
      normalized    = map (second (/ grandSum)) summed
      
      -- Get module information for each tick
      splitTs (n,f) = let (mo,n') = splitGlobalTick n in (mo, n', f)
      
  in map splitTs normalized

tagsFromLocalTicks :: Int -> EventsArray -> [Tag]
tagsFromLocalTicks startIx eventsArr = map toTag $ findLocalTicks startIx eventsArr
  where toTag (mod, tick, freq) = Tag {
          tagModule = Just mod,
          tagName   = Nothing,
          tagTick   = Just tick,
          tagFreq   = freq
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

-- | Lookup an instruction pointer in the range map.
lookupRange :: RangeMap -> Int -> Maybe ObjRange
lookupRange rangeMap ip 
  = case IM.splitLookup ip rangeMap of  
    (_, Just r, _)           -> Just r
    (lowerRanges, _, _)
      | IM.null lowerRanges  -> Nothing
      | (_, r) <- IM.findMax lowerRanges
        , ip < fI (raHigh r) -> Just r
      | otherwise            -> Nothing
  where fI = fromIntegral

buildRangeMap :: [ObjRange] -> IM.IntMap ObjRange
buildRangeMap ranges =
  let sorted = sortBy (compare `F.on` raLow) ranges
      
      -- Scans range list and inserts all ranges into a map. 
      down :: [ObjRange] -> [ObjRange] -> [(Integer, ObjRange)]
      down []     []              = []
      down (s:ss) []              = up ss []     (raHigh s)
      down []     (r:rs)          = (raLow r, r) : (down [r] rs)
      down (s:ss) (r:rs)
        | raHigh s <= raLow r     = up ss (r:rs) (raHigh s)
        | SymbolRange <- raType r =                 down (s:ss) rs
        | otherwise               = (raLow r, r) : (down (r:s:ss) rs)
      
      -- Called to remove items from the stack, maybe re-inserting
      -- some ranges that were overriden but still apply. Will revert
      -- to "down" behaviour once no more ranges can be popped from
      -- the stack.
      up :: [ObjRange] -> [ObjRange] -> Integer ->  [(Integer, ObjRange)]
      up []     rs _   =           down [] rs
      up (s:ss) rs p 
        | raHigh s > p = (p, s) : (down (s:ss) rs)
        | otherwise    =           up   ss rs p
  
  in IM.fromAscList $ map (first fromInteger) $ down [] sorted
      
findLocalIPsamples :: Int -> EventsArray -> [(Timestamp, [Word32])]
findLocalIPsamples startIx eventsArr =
  let getIPs CapEvent{{-ce_cap,-}ce_event=Event{time,spec=InstrPtrSample{..}}} 
        = Just ({- ce_cap, -}time, ips)
      getIPs _other
        = Nothing
  in findLocalEvents startIx ipSampleWinSize eventsArr getIPs

-- | Looks up a number of ranges, groups together same ranges
lookupRanges :: RangeMap -> [Word32] -> [(Int, ObjRange)]
lookupRanges rangeMap ips = 
  let ranges = mapMaybe (lookupRange rangeMap . fromIntegral) ips
      ord = compare `F.on` (raLow . snd)
      _ `plus` (n, r) = (n+1, r)
  in nubSumBy ord plus $ map ((,) 1) ranges

-- | Removes duplicates and allows for custom combination function
nubSumBy :: (a -> a -> Ordering) -> (a -> a -> a) -> [a] -> [a]
nubSumBy ord plus = map (foldr1 plus) . groupBy eq . sortBy ord
  where x `eq` y = ord x y == EQ

-- | Finds local IP samples, return weighted
findLocalIPsWeighted :: Int -> EventsArray -> RangeMap -> [(Double, ObjRange)]
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
      ord = compare `F.on` (raLow . snd)
      (w1, _) `plus` (w2, r) = (w1+w2, r)
  in nubSumBy ord plus wips

tagsFromLocalIPs :: Int -> EventsArray -> RangeMap -> UnitMap -> [Tag]
tagsFromLocalIPs startIx eventsArr rangeMap unitMap =
  let toTag freq ObjRange{..} = Tag {
        tagModule = case lookup raUnit unitMap of
           Just unit       -> Just unit
           _ | null raUnit -> Nothing   
             | otherwise   -> Just $ takeFileName raUnit,
        tagName = Just name,
        tagTick = fromIntegral <$> tk,
        tagFreq = freq
        } where (name, tk) = decodeName raName
                
      weighted = findLocalIPsWeighted startIx eventsArr rangeMap
      grandSum = sum $ map fst weighted
      tags = map (uncurry toTag . first (/grandSum)) weighted
      
      msr Tag{..}
        | Just ti <- tagTick  = Left ti
        | otherwise           = Right (tagModule, tagName)
      ord = compare `F.on` msr
      t1 `plus` t2 = t1 { tagFreq = tagFreq t1 + tagFreq t2 }
  in nubSumBy ord plus tags

-- | Names in the symbol table have some extra information in them, as
-- well as often standing for actual Haskell names. We try to process
-- and sanitize the names a bit here.
decodeName :: String -> (String, Maybe Int)
decodeName s = (sanitize s', m_i)
  where    
    
    -- This is *hilariously* crude
    (s', m_i)
      | Just (s', i) <- splt_go (reverse s) 0 1
        = (s', Just i)        
      | otherwise  = (s, Nothing)
    splt_go ('k':'c':'i':'t':'i':'_':cs) i _ = Just (reverse cs, i)
    splt_go (c:cs) i m | isDigit c = splt_go cs (i+m*(ord c-ord '0')) (m*10)
    splt_go _ _ _ = Nothing
    
    sanitize s
      | "_info" `isSuffixOf` s = zdecode $ take (length s - 5) s
      | "_static" `isSuffixOf` s = zdecode $ take (length s - 7) s
      | "_con" `isSuffixOf` s  = zdecode $ take (length s - 4) s
      | otherwise              = zdecode $ s
      
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

clearTextTags :: TextBuffer -> IO ()
clearTextTags sourceBuffer = do
  tagTable <- textBufferGetTagTable sourceBuffer
  textTagTableForeach tagTable (textTagTableRemove tagTable)  

showModule :: SourceView -> String -> IO ()
showModule view@SourceView{..} modName = do
  state <- readIORef stateRef
  clearTextTags sourceBuffer
  
  case state of
    StateLoaded{..} 
      | Just file <- lookup modName fileMap -> 
        when (currentMod /= Just modName) $ do
      
          -- Load the source file
          modSrc <- readFile file
          textBufferSetText sourceBuffer modSrc

          -- Set state
          writeIORef stateRef state{ currentMod = Just modName }

          -- Update tagging
          updateTextTags view
        
    StateLoaded{} -> do
      writeIORef stateRef state{ currentMod = Nothing }
      textBufferSetText sourceBuffer $
        " *** Could not find source code or Mix/Cix data. " ++
        "Make sure all of it is accessible from the event log's path! ***"
      
    StateEmpty -> do
      writeIORef stateRef state{ currentMod = Nothing }
      textBufferSetText sourceBuffer ""

updateTextTags :: SourceView -> IO ()
updateTextTags SourceView{..} = do
  
  state <- readIORef stateRef  
  case state of
    StateLoaded{..} 
      | Just modName <- currentMod
      , Just cixMap <- lookup modName cixMaps
      , Just mix <- lookup modName mixData -> do

        -- Clear existing tags
        clearTextTags sourceBuffer

        -- Use monospace
        setMonospaceFont sourceBuffer
        
        -- Annotate source code
        annotateTags sourceView sourceBuffer cixMap mix tags False
        case selection of
         Nothing  -> return ()
         Just sel -> annotateTags sourceView sourceBuffer cixMap mix [sel] True

    _ -> return ()
  
setMonospaceFont :: TextBuffer -> IO ()
setMonospaceFont sourceBuffer = do
  tagTable <- textBufferGetTagTable sourceBuffer  
  monoTag <- textTagNew (Just "Monospace")
  set monoTag [textTagFamily := "Monospace", textTagFamilySet := True]
  textTagTableAdd tagTable monoTag
  start <- textBufferGetStartIter sourceBuffer
  end <- textBufferGetEndIter sourceBuffer
  textBufferApplyTag sourceBuffer monoTag start end

annotateTags :: TextView -> TextBuffer -> CixMap -> Mix -> [Tag] -> Bool -> IO ()
annotateTags sourceView sourceBuffer cixMap (Mix _ _ _ _ mixe) tags sel = do
  tagTable <- textBufferGetTagTable sourceBuffer  
  
  let freqMap   = [(tagFreq, stick)
                  | Tag{tagFreq, tagTick = Just tick} <- tags
                  , Just sticks <- [lookup (fromIntegral tick) cixMap]
                  , stick <- sticks]
      --freqMap'  = groupBy ((==) `F.on` snd) $ sortBy (compare `F.on` snd) freqMap
      --freqMap'' = map (\fs -> (sum $ map fst fs, snd $ head fs)) freqMap'
      freqMap''   = nubSumBy (compare `F.on` snd) (\(f1, _) (f2, t) -> (f1+f2,t)) freqMap
  
  forM_ freqMap'' $ \(freq, stick) -> do
        
    -- Create color tag
    tag <- textTagNew Nothing
    let w = min 230 $ max 0 $ 255 - round (255 * freq)
        ws | w < 16 = '0': showHex w ""
           | True   = showHex w ""
        clr = if sel then "#ff" ++ ws ++ ws else "#" ++ ws ++ ws ++ "ff"
    set tag [textTagBackground := clr, textTagBackgroundSet := True]
    tagTable `textTagTableAdd` tag
      
      -- Place at code position
    let (sl, sc, el, ec) = fromHpcPos $ fst (mixe !! stick)
    -- print (stick, freq, sl, sc, el, ec, w)
    start <- textBufferGetIterAtLineOffset sourceBuffer (sl-1) (sc-1)
    end <- textBufferGetIterAtLineOffset sourceBuffer (el-1) ec
    textBufferApplyTag sourceBuffer tag start end
  
  -- Scroll to largest tick
  when (sel && not (null freqMap'')) $ do
    let ticks = map (fst . (mixe !!) . snd) freqMap''
        hpcSize p = let (sl, sc, el, ec) = fromHpcPos p in (el-sl, ec-sc)
        largestTick = maximumBy (compare `F.on` hpcSize) ticks
        (l, c, _, _) = fromHpcPos largestTick
    iter <- textBufferGetIterAtLineOffset sourceBuffer (l-1) (c-1)
    _ <- textViewScrollToIter sourceView iter 0.2 Nothing
    return ()
      
-------------------------------------------------------------------------------
  
  
-- | Takes Cix and Mix information and finds which instrumentation
-- ticks could be associated with which source ticks.
rawCixMap :: CixTree -> Mix -> (CixMap, CoreMap)
rawCixMap cix _mix = snd $ go [] cix
  where
    
    goSum :: [([Int], (CixMap, CoreMap))] -> ([Int], (CixMap, CoreMap))
    goSum = foldr plus ([],([],[]))
      where plus (ctxd1, (cxm1, cdm1)) (ctxd2, (cxm2, cdm2)) 
              = (ctxd1 ++ ctxd2, (cxm1 ++ cxm2, cdm1 ++ cdm2))
    
    getSrcTick (CixInfo CixSource _ n) = n
    getSrcTick (CixInfo CixInstrument _ _) = error "Funny place for an instrumentation tick..."
    
    -- Go through tree. For each instrumentation tick, give context:
    -- 1) All source ticks on the path to the root
    -- 2) All source ticks directly below
    -- Both stop on the first instrumentation tick that is encountered
    go :: [Int] -> CixTree -> ([Int], (CixMap, CoreMap))
    go ctx (CixTree ((CixInfo CixInstrument _ n):cis) cxs m_cd) =
      let srcs = map getSrcTick cis
          ctx' | null srcs = ctx          
               | otherwise = srcs
          (ctx2, (cxm, cdm)) = goSum $ map (go ctx') cxs
          cxm' = (n, nub $ ctx ++ ctx2):cxm
          cdm' | Just cd <- m_cd = (n, cd):cdm
               | otherwise       = cdm
      in ([], (cxm', cdm'))
    go ctx (CixTree cis cxs _) = 
      let srcs = map getSrcTick cis
          (ctx2, (cxm, cdm)) = goSum $ map (go (ctx ++ srcs)) cxs
      in (ctx2 ++ srcs, (cxm, cdm))

-- | Given a list of source tick IDs, eliminates all ranges that are a
-- subrange of another range in the array.
elimSubranges :: Mix -> [Int] -> [Int]
elimSubranges (Mix _ _ _ _ mixe) = go []
  where 
    go ns2 []     = ns2
    go ns2 (n:ns) = go (n:filter f ns2) (filter f ns)
      where f n2 = not (fst (mixe !! n2) `insideHpcPos` fst (mixe !! n))

-- | Like @rawCixMap@, but removes subranges in source ticks
mkCixMap :: CixTree -> Mix -> (CixMap, CoreMap)
mkCixMap cix mix = first (map (second (elimSubranges mix))) $ rawCixMap cix mix
    
-------------------------------------------------------------------------------

showCore :: SourceView -> IO ()
showCore SourceView{..} = do
  state <- readIORef stateRef  
  case state of
    StateLoaded{..} 
      | Just tag <- selection
      , Just n <- tagTick tag
      , Just core <- lookup (fromIntegral n) =<< lookup "Main" coreMaps -> do
        
        clearTextTags sourceBuffer
        textBufferSetText sourceBuffer core
        setMonospaceFont sourceBuffer

        writeIORef stateRef state { currentMod = Nothing }

    _ -> return ()

-------------------------------------------------------------------------------

-- This *really* ought to be in a standard library somwhere.
clampBounds :: Ord a => (a, a) -> a -> a
clampBounds (lower, upper) x
  | x <= lower = lower
  | x >  upper = upper
  | otherwise  = x
