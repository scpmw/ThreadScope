{-# LANGUAGE CPP #-}
module GUI.SaveAs (saveAsPDF, saveAsPNG) where

-- Imports for ThreadScope
import GUI.Timeline.Render (renderTraces, renderYScaleArea)
import GUI.Timeline.Render.Constants
import GUI.Timeline.Ticks (renderXScaleArea)
import GUI.Types
import Events.HECs

-- Imports for GTK
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

saveAs :: HECs -> ViewParameters -> Double -> (Int, Int, Render ())
saveAs hecs params' @ViewParameters{xScaleAreaHeight, width,
                                    height = oldHeight, histogramHeight}
       yScaleAreaWidth =
  let histTotalHeight = histogramHeight + xScaleAreaHeight
      params@ViewParameters{height} =
#ifdef USE_SPARK_HISTOGRAM
        params'{ viewTraces = viewTraces params' ++ [TraceHistogram]
               , height = oldHeight + histTotalHeight + tracePad
               }
#else
        params'
#endif
      w = ceiling yScaleAreaWidth + width
      h = xScaleAreaHeight + height
      drawTraces = renderTraces params hecs (Rectangle 0 0 width height)
      drawXScale = renderXScaleArea params hecs True
      drawYScale = renderYScaleArea params hecs yScaleAreaWidth
      -- Functions renderTraces and renderXScaleArea draw to the left of 0
      -- which is not seen in the normal mode, but would be seen in export,
      -- so it has to be cleared before renderYScaleArea is written on top:
      clearLeftArea = do
        rectangle 0 0 yScaleAreaWidth (fromIntegral h)
        op <- getOperator
        setOperator OperatorClear
        fill
        setOperator op
      drawAll = do
        translate yScaleAreaWidth (fromIntegral xScaleAreaHeight)
        drawTraces
        translate 0 (- fromIntegral xScaleAreaHeight)
        drawXScale
        translate (-yScaleAreaWidth) 0
        clearLeftArea
        translate 0 (fromIntegral xScaleAreaHeight)
        drawYScale
  in (w, h, drawAll)

saveAsPDF :: FilePath -> HECs -> ViewParameters -> Double -> IO ()
saveAsPDF filename hecs params yScaleAreaWidth =
  let (w', h', drawAll) = saveAs hecs params yScaleAreaWidth
  in withPDFSurface filename (fromIntegral w') (fromIntegral h') $ \surface ->
       renderWith surface drawAll

saveAsPNG :: FilePath -> HECs -> ViewParameters -> Double -> IO ()
saveAsPNG filename hecs params yScaleAreaWidth =
  let (w', h', drawAll) = saveAs hecs params yScaleAreaWidth
  in withImageSurface FormatARGB32 w' h' $ \surface -> do
       renderWith surface drawAll
       surfaceWriteToPNG surface filename
