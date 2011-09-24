{-# LANGUAGE NamedFieldPuns #-}
module GUI.Timeline.Render (
    renderView,
    renderTraces,
    updateYScaleArea,
    updateXScaleArea,
    calculateTotalTimelineHeight,
    toWholePixels,
    renderYScaleArea,
    renderXScaleArea,
  ) where

import GUI.Timeline.Types
import GUI.Timeline.Render.Constants
import GUI.Timeline.Ticks (renderXScale, renderYScale, renderVRulers)
import GUI.Timeline.HEC
import GUI.Timeline.Sparks
import GUI.Timeline.Activity

import Events.HECs
import GUI.Types
import GUI.ViewerColours
import GUI.Timeline.CairoDrawing

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

import Data.IORef
import Control.Monad

-------------------------------------------------------------------------------

-- | This function redraws the currently visible part of the
--   main trace canvas plus related canvases.
--
renderView :: TimelineState
           -> ViewParameters
           -> HECs -> TimeSelection -> [Timestamp]
           -> Region -> IO ()
renderView TimelineState{timelineDrawingArea, timelineVAdj, timelinePrevView}
           params hecs selection bookmarks exposeRegion = do

  -- Get state information from user-interface components
  (w, _) <- widgetGetSize timelineDrawingArea
  vadj_value <- adjustmentGetValue timelineVAdj

  prev_view <- readIORef timelinePrevView

  rect <- regionGetClipbox exposeRegion

  win <- widgetGetDrawWindow timelineDrawingArea
  renderWithDrawable win $ do

  let renderToNewSurface = do
        new_surface <- withTargetSurface $ \surface ->
          liftIO $ createSimilarSurface surface ContentColor w (height params)
        renderWith new_surface $ do
          clearWhite
          renderTraces params hecs rect
        return new_surface

  surface <-
    case prev_view of
      Nothing -> renderToNewSurface

      Just (old_params, surface)
         | old_params == params
         -> return surface

         | width  old_params == width  params &&
           height old_params == height params
         -> do
               if old_params { hadjValue = hadjValue params } == params
                  -- only the hadjValue changed
                  && abs (hadjValue params - hadjValue old_params) <
                     fromIntegral (width params) * scaleValue params
                  -- and the views overlap...
                  then do
                       scrollView surface old_params params hecs

                  else do
                       renderWith surface $ do
                          clearWhite; renderTraces params hecs rect
                       return surface

         | otherwise
         -> do surfaceFinish surface
               renderToNewSurface

  liftIO $ writeIORef timelinePrevView (Just (params, surface))

  region exposeRegion
  clip
  setSourceSurface surface 0 (-vadj_value)
          -- ^^ this is where we adjust for the vertical scrollbar
  setOperator OperatorSource
  paint
  renderBookmarks bookmarks params
  drawSelection params selection

-------------------------------------------------------------------------------

-- Render the bookmarks
renderBookmarks :: [Timestamp] -> ViewParameters -> Render ()
renderBookmarks bookmarks vp@ViewParameters{height} = do
    setLineWidth 1
    setSourceRGBAhex bookmarkColour 1.0
    sequence_
      [ do moveTo x 0
           lineTo x (fromIntegral height)
           stroke
      | bookmark <- bookmarks
      , let x = timestampToView vp bookmark ]

-------------------------------------------------------------------------------

drawSelection :: ViewParameters -> TimeSelection -> Render ()
drawSelection vp@ViewParameters{height} (PointSelection x) = do
    setLineWidth 3
    setOperator OperatorOver
    setSourceRGBAhex blue 1.0
    moveTo xv 0
    lineTo xv (fromIntegral height)
    stroke
  where
    xv = timestampToView vp x

drawSelection vp@ViewParameters{height} (RangeSelection x x') = do
    setLineWidth 1.5
    setOperator OperatorOver

    setSourceRGBAhex blue 0.25
    rectangle xv 0 (xv' - xv) (fromIntegral height)
    fill

    setSourceRGBAhex blue 1.0
    moveTo xv 0
    lineTo xv (fromIntegral height)
    moveTo xv' 0
    lineTo xv' (fromIntegral height)
    stroke
  where
    xv  = timestampToView vp x
    xv' = timestampToView vp x'

-------------------------------------------------------------------------------

-- We currently have two different way of converting from logical units
-- (ie timestamps in nanoseconds) to device units (ie pixels):
--   * the first is to set the cairo context to the appropriate scale
--   * the second is to do the conversion ourself
--
-- While in principle the first is superior due to the simplicity: cairo
-- lets us use Double as the logical unit and scaling factor. In practice
-- however cairo does not support the full Double range because internally
-- it makes use of a 32bit fixed point float format. With very large scaling
-- factors we end up with artifacts like lines disappearing.
--
-- So sadly we will probably have to convert to using the second method.

-- | Use cairo to convert from logical units (timestamps) to device units
--
withViewScale :: ViewParameters -> Render () -> Render ()
withViewScale ViewParameters{scaleValue, hadjValue} inner = do
   save
   scale (1/scaleValue) 1.0
   translate (-hadjValue) 0
   inner
   restore

-- | Manually convert from logical units (timestamps) to device units.
--
timestampToView :: ViewParameters -> Timestamp -> Double
timestampToView ViewParameters{scaleValue, hadjValue} ts =
  (fromIntegral ts - hadjValue) / scaleValue

-------------------------------------------------------------------------------
-- This function draws the current view of all the HECs with Cairo.

renderTraces :: ViewParameters -> HECs -> Rectangle
             -> Render ()

renderTraces params@ViewParameters{..} hecs (Rectangle rx _ry rw _rh) =
  do
    let scale_rx    = fromIntegral rx * scaleValue
        scale_rw    = fromIntegral rw * scaleValue
        scale_width = fromIntegral width * scaleValue

        startPos :: Timestamp
        startPos = fromIntegral $ truncate (scale_rx + hadjValue)

        endPos :: Timestamp
        endPos = minimum [
                   ceiling (hadjValue + scale_width),
                   ceiling (hadjValue + scale_rx + scale_rw),
                   hecLastEventTime hecs
                ]

        -- Round the start time down, and the end time up,
        -- to a slice boundary:
        start = (startPos `div` slice) * slice
        end = ((endPos + slice) `div` slice) * slice
        (slice, prof) = treesProfile scaleValue start end hecs

    withViewScale params $ do
      -- Render the vertical rulers across all the traces.
      renderVRulers startPos endPos scaleValue height

      -- This function helps to render a single HEC.
      -- Traces are rendered even if the y-region falls outside visible area.
      -- OTOH, trace rendering function tend to drawn only the visible
      -- x-region of the graph.
      let renderTrace trace y = do
            save
            translate 0 (fromIntegral y)
            case trace of
               TraceHEC c ->
                 let (dtree, etree, _) = hecTrees hecs !! c
                 in renderHEC params startPos endPos (dtree, etree)
               SparkCreationHEC c ->
                 renderSparkCreation params slice start end (prof !! c)
               SparkConversionHEC c ->
                 renderSparkConversion params slice start end (prof !! c)
               SparkPoolHEC c ->
                 let maxP = maxSparkPool hecs
                 in renderSparkPool params slice start end (prof !! c) maxP
               TraceActivity ->
                 renderActivity params hecs startPos endPos
               _   ->
                 return ()
            restore
      -- Now rennder all the HECs.
      zipWithM_ renderTrace viewTraces (traceYPositions labelsMode viewTraces)

-------------------------------------------------------------------------------

-- parameters differ only in the hadjValue, we can scroll ...
scrollView :: Surface
           -> ViewParameters -> ViewParameters
           -> HECs
           -> Render Surface

scrollView surface old new hecs = do

--   scrolling on the same surface seems not to work, I get garbled results.
--   Not sure what the best way to do this is.
--   let new_surface = surface
   new_surface <- withTargetSurface $ \surface ->
                    liftIO $ createSimilarSurface surface ContentColor
                                (width new) (height new)

   renderWith new_surface $ do

       let
           scale    = scaleValue new
           old_hadj = hadjValue old
           new_hadj = hadjValue new
           w        = fromIntegral (width new)
           h        = fromIntegral (height new)
           off      = (old_hadj - new_hadj) / scale

--   liftIO $ printf "scrollView: old: %f, new %f, dist = %f (%f pixels)\n"
--              old_hadj new_hadj (old_hadj - new_hadj) off

       -- copy the content from the old surface to the new surface,
       -- shifted by the appropriate amount.
       setSourceSurface surface off 0
       if old_hadj > new_hadj
          then do rectangle off 0 (w - off) h -- scroll right.
          else do rectangle 0   0 (w + off) h -- scroll left.
       fill

       let rect | old_hadj > new_hadj
                = Rectangle 0 0 (ceiling off) (height new)
                | otherwise
                = Rectangle (truncate (w + off)) 0 (ceiling (-off)) (height new)

       case rect of
         Rectangle x y w h -> rectangle (fromIntegral x) (fromIntegral y)
                                        (fromIntegral w) (fromIntegral h)
       setSourceRGBA 0xffff 0xffff 0xffff 0xffff
       fill

       renderTraces new hecs rect

   surfaceFinish surface
   return new_surface

-------------------------------------------------------------------------------

-- TODO: refactor all below (deduplicate, move)
renderYScaleArea :: ViewParameters -> HECs -> Double -> Render ()
renderYScaleArea ViewParameters{..} hecs xoffset =
  drawYScaleArea maxSpkValue (maxSparkPool hecs) xoffset
    0 labelsMode viewTraces

renderXScaleArea :: ViewParameters -> HECs -> Int -> Render ()
renderXScaleArea  ViewParameters{..} hecs yoffset = do
  let scale_rw = fromIntegral width * scaleValue
      startPos :: Timestamp
      startPos = truncate hadjValue
      lastTx = hecLastEventTime hecs
      endPos :: Timestamp
      endPos = minimum [ceiling (hadjValue + scale_rw), lastTx]
  save
  scale (1/scaleValue) 1.0
  translate (-hadjValue) 0
  renderXScale startPos endPos scaleValue yoffset
  restore

updateYScaleArea :: TimelineState -> Double -> Bool -> [Trace] -> IO ()
updateYScaleArea TimelineState{..} maxSparkPool showLabels traces = do
  win <- widgetGetDrawWindow timelineYScaleArea
  maxSpkValue  <- readIORef maxSpkIORef
  vadj_value   <- adjustmentGetValue timelineVAdj
  (xoffset, _) <- widgetGetSize timelineYScaleArea
  renderWithDrawable win $
    drawYScaleArea maxSpkValue maxSparkPool (fromIntegral xoffset)
      vadj_value showLabels traces

-- For simplicity, unlike for the traces, we redraw the whole area,
-- not only the newly exposed one.
updateXScaleArea :: TimelineState -> Timestamp -> IO ()
updateXScaleArea TimelineState{..} lastTx = do
  win <- widgetGetDrawWindow timelineXScaleArea
  (rw, _) <- widgetGetSize timelineDrawingArea
  (_, yoffset) <- widgetGetSize timelineXScaleArea
  scaleValue <- readIORef scaleIORef
  -- snap the view to whole pixels, to avoid blurring
  hadjValue0 <- adjustmentGetValue timelineAdj
  let hadjValue = toWholePixels scaleValue hadjValue0
      scale_rw = fromIntegral rw * scaleValue
      startPos :: Timestamp
      startPos = truncate hadjValue
      endPos :: Timestamp
      endPos = minimum [ceiling (hadjValue + scale_rw), lastTx]
  renderWithDrawable win $ do
    save
    scale (1/scaleValue) 1.0
    translate (-hadjValue) 0
    renderXScale startPos endPos scaleValue yoffset
    restore
  return ()

drawYScaleArea :: Double -> Double -> Double ->  Double -> Bool -> [Trace]
                        -> Render ()
drawYScaleArea maxSpkValue maxSparkPool xoffset vadj_value showLabels traces =
  let ys = map (subtract (round vadj_value)) $
             traceYPositions showLabels traces
  in zipWithM_ (drawSingleYScale maxSpkValue maxSparkPool xoffset) traces ys

drawSingleYScale :: Double -> Double -> Double -> Trace -> Int -> Render ()
drawSingleYScale maxSpkValue maxSparkPool xoffset trace y = do
  setSourceRGBAhex black 1
  move_to (ox, y + 8)
  layout <- createLayout $ showTrace trace
  liftIO $ do
    layoutSetWidth layout (Just $ xoffset - 50)
    layoutSetAttributes layout [AttrSize minBound maxBound 8,
                                AttrFamily minBound maxBound "sans serif"]
  showLayout layout
  case traceMaxSpark maxSpkValue maxSparkPool trace of
    Just v  -> renderYScale hecSparksHeight 1 v (xoffset - 13) (fromIntegral y)
    Nothing -> return ()

--------------------------------------------------------------------------------

traceYPositions :: Bool -> [Trace] -> [Int]
traceYPositions showLabels traces =
  scanl (\a b -> a + (traceHeight b) + extra + tracePad) firstTraceY traces
    where
      extra = if showLabels then hecLabelExtra else 0
      traceHeight TraceHEC{}           = hecTraceHeight
      traceHeight SparkCreationHEC{}   = hecSparksHeight
      traceHeight SparkConversionHEC{} = hecSparksHeight
      traceHeight SparkPoolHEC{}       = hecSparksHeight
      traceHeight TraceActivity        = activityGraphHeight
      traceHeight _ = 0

calculateTotalTimelineHeight :: Bool -> [Trace] -> Int
calculateTotalTimelineHeight showLabels traces =
 last (traceYPositions showLabels traces)

showTrace :: Trace -> String
showTrace (TraceHEC n) =
  "HEC " ++ show n
showTrace (SparkCreationHEC n) =
  "\nHEC " ++ show n ++ "\n\nSpark creation rate (spark/ms)"
showTrace (SparkConversionHEC n) =
  "\nHEC " ++ show n ++ "\n\nSpark conversion rate (spark/ms)"
showTrace (SparkPoolHEC n) =
  "\nHEC " ++ show n ++ "\n\nSpark pool size"
showTrace TraceActivity =
  "Activity"
showTrace _ = error "Render.showTrace"

traceMaxSpark :: Double -> Double -> Trace -> Maybe Double
traceMaxSpark maxS _ SparkCreationHEC{}   = Just $ maxS * 1000000
traceMaxSpark maxS _ SparkConversionHEC{} = Just $ maxS * 1000000
traceMaxSpark _ maxP SparkPoolHEC{}       = Just $ maxP
traceMaxSpark _ _ _ = Nothing

toWholePixels :: Double -> Double -> Double
toWholePixels 0     _ = 0
toWholePixels scale x = fromIntegral (truncate (x / scale)) * scale
