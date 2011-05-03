{-# LANGUAGE NamedFieldPuns #-}
module GUI.Timeline.Render (
    renderView,
    renderTraces,
    updateLabelDrawingArea,
    calculateTotalTimelineHeight,
    toWholePixels
  ) where

import GUI.Timeline.Types
import GUI.Timeline.Render.Constants
import GUI.Timeline.Ticks
import GUI.Timeline.HEC
import GUI.Timeline.Activity

import GUI.Types
import GUI.ViewerColours
import GUI.Timeline.CairoDrawing

import GHC.RTS.Events hiding (Event)

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.GC (GC, gcNew) --FIXME: eliminate old style drawing

import Data.IORef
import Control.Monad

-------------------------------------------------------------------------------

-- | This function redraws the currently visible part of the
--   main trace canvas plus related canvases.
--
renderView :: TimelineWindow
           -> ViewParameters
           -> HECs -> Timestamp -> [Timestamp]
           -> Region -> IO ()
renderView TimelineWindow{timelineDrawingArea, timelineVAdj, timelinePrevView}
           params hecs cursor_t bookmarks exposeRegion = do

  -- Get state information from user-interface components
  (dAreaWidth, _) <- widgetGetSize timelineDrawingArea
  vadj_value <- adjustmentGetValue timelineVAdj

  prev_view <- readIORef timelinePrevView

  rect <- regionGetClipbox exposeRegion

  win <- widgetGetDrawWindow timelineDrawingArea
  renderWithDrawable win $ do

  let renderToNewSurface = do
        new_surface <- withTargetSurface $ \surface ->
                         liftIO $ createSimilarSurface surface ContentColor
                                    dAreaWidth (height params)
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
  when (scaleValue params > 0) $
    withViewScale params $ do
      renderBookmarks bookmarks params
      drawCursor cursor_t params

-------------------------------------------------------------------------------

renderBookmarks :: [Timestamp] -> ViewParameters -> Render ()
renderBookmarks bookmarks ViewParameters{height} = do
    -- Render the bookmarks
    -- First set the line width to one pixel and set the line colour
    (onePixel, _) <- deviceToUserDistance 1 0
    setLineWidth onePixel
    setSourceRGBAhex bookmarkColour 1.0
    sequence_
      [ draw_line (bookmark, 0) (bookmark, height)
      | bookmark <- bookmarks ]

-------------------------------------------------------------------------------

drawCursor :: Timestamp -> ViewParameters -> Render ()
drawCursor cursor_t ViewParameters{height} = do
    (threePixels, _) <- deviceToUserDistance 3 0
    setLineWidth threePixels
    setOperator OperatorOver
    setSourceRGBAhex blue 1.0
    moveTo (fromIntegral cursor_t) 0
    lineTo (fromIntegral cursor_t) (fromIntegral height)
    stroke

-------------------------------------------------------------------------------

withViewScale :: ViewParameters -> Render () -> Render ()
withViewScale ViewParameters{scaleValue, hadjValue} inner = do
   save
   scale (1/scaleValue) 1.0
   translate (-hadjValue) 0
   inner
   restore

-------------------------------------------------------------------------------
-- This function draws the current view of all the HECs with Cario

renderTraces :: ViewParameters -> HECs -> Rectangle
             -> Render ()

renderTraces params@ViewParameters{..} hecs (Rectangle rx _ry rw _rh)
  = do
    let
        scale_rx    = fromIntegral rx * scaleValue
        scale_rw    = fromIntegral rw * scaleValue
        scale_width = fromIntegral width * scaleValue

        startPos :: Timestamp
        startPos = fromIntegral (max 0 (truncate (scale_rx + hadjValue)))
                   -- hadj_value might be negative, as we leave a
                   -- small gap before the trace starts at the beginning

        endPos :: Timestamp
        endPos = minimum [
                   ceiling (max 0 (hadjValue + scale_width)),
                   ceiling (max 0 (hadjValue + scale_rx + scale_rw)),
                   hecLastEventTime hecs
                ]

    -- Now render the timeline drawing if we have a non-empty trace
    when (scaleValue > 0) $ do
      withViewScale params $ do
      save
      -- First render the ticks and tick times
      renderTicks startPos endPos scaleValue height
      restore

      -- This function helps to render a single HEC...
      let
        renderTrace trace y = do
            save
            translate 0 (fromIntegral y)
            case trace of
               TraceHEC c ->
                   renderHEC c params startPos endPos (hecTrees hecs !! c)
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

------------------------------------------------------------------------------

toWholePixels :: Double -> Double -> Double
toWholePixels 0    _x = 0
toWholePixels scale x = fromIntegral (truncate (x / scale)) * scale

-------------------------------------------------------------------------------

updateLabelDrawingArea :: TimelineWindow -> IO ()
updateLabelDrawingArea TimelineWindow{timelineVAdj, timelineLabelDrawingArea, showLabelsIORef, tracesIORef}
   = do traces <- readIORef tracesIORef
        labels_mode <- readIORef showLabelsIORef
        win <- widgetGetDrawWindow timelineLabelDrawingArea
        vadj_value <- adjustmentGetValue timelineVAdj
        gc <- gcNew win
        let ys = map (subtract (round vadj_value)) $
                      traceYPositions labels_mode traces
        zipWithM_ (drawLabel timelineLabelDrawingArea gc) traces ys

drawLabel :: DrawingArea -> GC -> Trace -> Int -> IO ()
drawLabel canvas gc trace y
  = do win <- widgetGetDrawWindow canvas
       txt <- canvas `widgetCreateLayout` (showTrace trace)
       --FIXME: eliminate use of GC drawing and use cairo instead.
       drawLayoutWithColors win gc 10 y txt (Just black) Nothing

--------------------------------------------------------------------------------

traceYPositions :: Bool -> [Trace] -> [Int]
traceYPositions labels_mode traces
  = scanl (\a b -> a + (traceHeight b) + extra + tracePad) firstTraceY traces
  where
      extra = if labels_mode then hecLabelExtra else 0

      traceHeight (TraceHEC _) = hecTraceHeight
      traceHeight TraceActivity = activityGraphHeight
      traceHeight _            = 0

--------------------------------------------------------------------------------

showTrace :: Trace -> String
showTrace (TraceHEC n)  = "HEC " ++ show n
showTrace TraceActivity = "Activity"
showTrace _            = "?"

--------------------------------------------------------------------------------

calculateTotalTimelineHeight :: TimelineWindow -> IO Int
calculateTotalTimelineHeight TimelineWindow{showLabelsIORef, tracesIORef} = do
   traces <- readIORef tracesIORef
   labels_mode <- readIORef showLabelsIORef
   return $ last (traceYPositions labels_mode traces)

--------------------------------------------------------------------------------
