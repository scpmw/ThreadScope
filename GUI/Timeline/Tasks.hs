
module GUI.Timeline.Tasks (renderTasks) where

import GUI.Timeline.CairoDrawing
import GUI.Timeline.Render.Constants
import GUI.Types
import GUI.ViewerColours

import Events.HECs
import Events.Tasks

import Graphics.Rendering.Cairo

import qualified Data.IntMap as IM

import Control.Monad

renderTasks :: ViewParameters -> HECs -> Timestamp -> Timestamp
            -> Render ()
renderTasks ViewParameters{..} HECs{taskGraph, taskLayout} start end = do
  let taskRowHgt = taskBarHeight + taskBarSpace

  setLineWidth (1/scaleValue)

  forM_ (IM.assocs taskLayout) $ \(i, y) -> do

    -- Activity bar
    let nd = taskGraph IM.! i
        x0 = max start (taskStart nd)
        x1 = min end (taskEnd nd)
        y0 = y * taskRowHgt
    when (x0 < x1) $ do
      setSourceRGBAhex (if not bwMode then runningColour else black) 1.0
      draw_rectangle x0 y0
                     (x1-x0) taskBarHeight

    -- Left context
    let lx = maximum $ start : map (taskEnd . (taskGraph IM.!)) (taskParents nd)
    when (lx < x0) $ do
      setLineWidth 1
      setSourceRGBAhex black 1.0
      draw_line (lx, y0 + taskBarHeight `div` 2)
                (x0, y0 + taskBarHeight `div` 2)

    -- Right context
    let rx = minimum $ end : map (taskStart . (taskGraph IM.!)) (taskChilds nd)
    when (rx > x1) $ do
      setLineWidth 1
      setSourceRGBAhex black 1.0
      draw_line (x1, y0 + taskBarHeight `div` 2)
                (rx, y0 + taskBarHeight `div` 2)
