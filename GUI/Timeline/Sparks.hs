{-# LANGUAGE CPP #-}
module GUI.Timeline.Sparks (
    treesProfile,
    maxSparkRenderedValue,
    renderSparkCreation,
    renderSparkConversion,
    renderSparkPool,
    Interval,
    renderSparkHistogram,
  ) where

import GUI.Timeline.Render.Constants

import Events.HECs
import Events.SparkTree
import qualified Events.SparkStats as SparkStats

import GUI.Types
import GUI.ViewerColours
import GUI.Timeline.Ticks (mu, deZero, renderHRulers, renderXScaleArea)

import Graphics.Rendering.Cairo

import qualified Data.List as L
import qualified Data.IntMap as IM
import Data.Accessor
import Text.Printf

import qualified Graphics.Rendering.Chart as Chart
import qualified Graphics.Rendering.Chart.Renderable as ChartR
import qualified Graphics.Rendering.Chart.Plot.Hidden as ChartH

-- import Text.Printf

-- Rendering sparks. No approximation nor extrapolation is going on here.
-- The sample data, recalculated for a given slice size in sparkProfile,
-- before these functions are called, is straightforwardly rendered.

maxSparkRenderedValue :: Timestamp -> SparkStats.SparkStats -> Double
maxSparkRenderedValue duration c =
  max (SparkStats.rateDud c +
       SparkStats.rateCreated c +
       SparkStats.rateOverflowed c)
      (SparkStats.rateFizzled c +
       SparkStats.rateConverted c +
       SparkStats.rateGCd c)
  / fromIntegral duration

spark_detail :: Int
spark_detail = 4 -- in pixels

treesProfile :: Double -> Timestamp -> Timestamp -> HECs
                -> (Timestamp, [[SparkStats.SparkStats]])
treesProfile scale start end hecs =
  let slice = round (fromIntegral spark_detail * scale)
      pr trees = let (_, _, stree) = trees
                 in sparkProfile slice start end stree
  in (slice, map pr (hecTrees hecs))


renderSparkCreation :: ViewParameters -> Timestamp -> Timestamp -> Timestamp
                       -> [SparkStats.SparkStats]
                       -> Render ()
renderSparkCreation params !slice !start !end prof = do
  let f1 c =        SparkStats.rateDud c
      f2 c = f1 c + SparkStats.rateCreated c
      f3 c = f2 c + SparkStats.rateOverflowed c
  renderSpark params slice start end prof
    f1 fizzledDudsColour f2 createdConvertedColour f3 overflowedColour

renderSparkConversion :: ViewParameters -> Timestamp -> Timestamp -> Timestamp
                         -> [SparkStats.SparkStats]
                         -> Render ()
renderSparkConversion params !slice !start !end prof = do
  let f1 c =        SparkStats.rateFizzled c
      f2 c = f1 c + SparkStats.rateGCd c
      f3 c = f2 c + SparkStats.rateConverted c
  renderSpark params slice start end prof
    f1 fizzledDudsColour f2 gcColour f3 createdConvertedColour

renderSparkPool :: ViewParameters -> Timestamp -> Timestamp -> Timestamp
                   -> [SparkStats.SparkStats]
                   -> Double -> Render ()
renderSparkPool ViewParameters{..} !slice !start !end prof !maxSparkPool = do
  let f1 c = SparkStats.minPool c
      f2 c = SparkStats.meanPool c
      f3 c = SparkStats.maxPool c
  addSparks outerPercentilesColour maxSparkPool f1 f2 start slice prof
  addSparks outerPercentilesColour maxSparkPool f2 f3 start slice prof
  outlineSparks maxSparkPool f2 start slice prof
  outlineSparks maxSparkPool (const 0) start slice prof
  renderHRulers hecSparksHeight start end

renderSpark :: ViewParameters -> Timestamp -> Timestamp -> Timestamp
               -> [SparkStats.SparkStats]
               -> (SparkStats.SparkStats -> Double) -> Color
               -> (SparkStats.SparkStats -> Double) -> Color
               -> (SparkStats.SparkStats -> Double) -> Color
               -> Render ()
renderSpark ViewParameters{..} slice start end prof f1 c1 f2 c2 f3 c3 = do
  -- maxSpkValue is maximal spark transition rate, so
  -- maxSliceSpark is maximal number of sparks per slice for current data.
  let maxSliceSpark = maxSpkValue * fromIntegral slice
  outlineSparks maxSliceSpark f3 start slice prof
  addSparks c1 maxSliceSpark (const 0) f1 start slice prof
  addSparks c2 maxSliceSpark f1 f2 start slice prof
  addSparks c3 maxSliceSpark f2 f3 start slice prof
  renderHRulers hecSparksHeight start end

off :: Double -> (SparkStats.SparkStats -> Double)
       -> SparkStats.SparkStats
       -> Double
off maxSliceSpark f t =
  let clipped = min 1 (f t / maxSliceSpark)
  in fromIntegral hecSparksHeight * (1 - clipped)

outlineSparks :: Double
                 -> (SparkStats.SparkStats -> Double)
                 -> Timestamp -> Timestamp
                 -> [SparkStats.SparkStats]
                 -> Render ()
outlineSparks maxSliceSpark f start slice ts = do
  case ts of
    [] -> return ()
    ts -> do
      let dstart = fromIntegral start
          dslice = fromIntegral slice
          points = [dstart-dslice/2, dstart+dslice/2 ..]
          t = zip points (map (off maxSliceSpark f) ts)
      newPath
      moveTo (dstart-dslice/2) (snd $ head t)
      mapM_ (uncurry lineTo) t
      setSourceRGBAhex black 1.0
      setLineWidth 1
      stroke

addSparks :: Color
             -> Double
             -> (SparkStats.SparkStats -> Double)
             -> (SparkStats.SparkStats -> Double)
             -> Timestamp -> Timestamp
             -> [SparkStats.SparkStats]
             -> Render ()
addSparks colour maxSliceSpark f0 f1 start slice ts = do
  case ts of
    [] -> return ()
    ts -> do
      -- liftIO $ printf "ts: %s\n" (show (map f1 (ts)))
      -- liftIO $ printf "off: %s\n"
      --   (show (map (off maxSliceSpark f1) (ts) :: [Double]))
      let dstart = fromIntegral start
          dslice = fromIntegral slice
          points = [dstart-dslice/2, dstart+dslice/2 ..]
          t0 = zip points (map (off maxSliceSpark f0) ts)
          t1 = zip points (map (off maxSliceSpark f1) ts)
      newPath
      moveTo (dstart-dslice/2) (snd $ head t1)
      mapM_ (uncurry lineTo) t1
      mapM_ (uncurry lineTo) (reverse t0)
      setSourceRGBAhex colour 1.0
      fill

type Interval = (Timestamp, Timestamp)

renderSparkHistogram :: ViewParameters -> HECs -> Maybe Interval -> Render ()
renderSparkHistogram params@ViewParameters{..} hecs minterval =
  let intDoub :: Integral a => a -> Double
      intDoub = fromIntegral
      histo :: [(Int, Timestamp)] -> [(Int, Timestamp)]
      histo durs = IM.toList $ fromListWith' (+) durs
      inR :: Timestamp -> Bool
      inR = case minterval of
              Nothing -> const True
              Just (from, to) -> \ t -> t >= from && t <= to
      -- TODO: if xs is sorted, we can slightly optimize the filtering
      inRange :: [(Timestamp, Int, Timestamp)] -> [(Int, Timestamp)]
      inRange xs = [(logdur, dur)
                   | (start, logdur, dur) <- xs, inR start]
      plot :: [(Timestamp, Int, Timestamp)] -> Chart.Layout1 Double Double
      plot xs =
        let layout = Chart.layout1_plots ^= [Left plot]
                   $ Chart.layout1_left_axis ^= yaxis
                   $ Chart.layout1_bottom_axis ^= xaxis
                   $ Chart.defaultLayout1 :: Chart.Layout1 Double Double
            yaxis  = Chart.laxis_title ^= ""
                   $ Chart.laxis_override ^= Chart.axis_labels ^: map override0
                   $ Chart.defaultLayoutAxis
            xaxis  = Chart.laxis_title ^= ""
                   $ Chart.laxis_override ^= Chart.axis_labels ^: map override0
                   $ Chart.defaultLayoutAxis
            ytitle = "Total duration (" ++ mu ++ "s)"
            xtitle = "Individual spark duration (" ++ mu ++ "s)"
            override0 d = [ (x, "") | (x, _) <- d]
            overrideX d = [ (x, deZero (printf "%.4f" (10 ** (x / 5) / 1000)))
                          | (x, _) <- d]  -- TODO: round it up before **
            plot = Chart.joinPlot plotBars plotHidden
            plotHidden =  -- to fix the x an y scales
              Chart.toPlot $ ChartH.PlotHidden
                [intDoub (minXHistogram hecs), intDoub (maxXHistogram hecs)]
                [0, intDoub (maxYHistogram hecs) / 1000]
            plotBars = Chart.plotBars bars
            bars = Chart.plot_bars_values ^= barvs $ Chart.defaultPlotBars
            barvs = [(intDoub t, [intDoub height / 1000])
                    | (t, height) <- histo $ inRange xs]
        in layout
      xs = durHistogram hecs
      renderable :: Chart.Renderable ()
      renderable = ChartR.toRenderable (plot xs)
  in do
       let drawHist =
             Chart.runCRender (Chart.render renderable size) ChartR.bitmapEnv
           drawXScale = renderXScaleArea params hecs (ceiling xScaleAreaHeight)
           xScaleAreaHeight = 45 -- TODO: should not be hardcoded, get it from "timeline_xscale_area"
           size = (fromIntegral width,
                   fromIntegral histogramHeight - xScaleAreaHeight)
           mult = 1000  -- HACK for PNG/PDF export: clear rulers everywhere below
       rectangle 0 (- fromIntegral firstTraceY) (fst size) (mult * snd size)
       setSourceRGBAhex white 1.0
       fill
       drawHist
       translate 0 (snd size)
       drawXScale

-- TODO: factor out to module with helper stuff (mu, deZero, this)
fromListWith' :: (a -> a -> a) -> [(Int, a)] -> IM.IntMap a
fromListWith' f xs =
    L.foldl' ins IM.empty xs
  where
#if MIN_VERSION_containers(0,4,1)
    ins t (k,x) = IM.insertWith' f k x t
#else
    ins t (k,x) =
      let r = IM.insertWith f k x t
          v = r IM.! k
      in v `seq` r
#endif
