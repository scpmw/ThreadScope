
module GUI.Timeline.Tasks (renderTasks) where

import GUI.Timeline.CairoDrawing
import GUI.Timeline.Render.Constants
import GUI.Types
import GUI.ViewerColours

import Events.HECs
import Events.Tasks

import Graphics.Rendering.Cairo

import qualified Data.IntMap as IM
import Data.List (minimumBy, maximumBy)
import Data.Ord (comparing)

import Control.Monad

renderTasks :: ViewParameters -> HECs -> Timestamp -> Timestamp
            -> Render ()
renderTasks ViewParameters{..} HECs{taskGraph, taskLayout} start end = do
  let taskRowHgt = taskBarHeight + taskBarSpace

  forM_ (IM.assocs taskLayout) $ \(i, (y, _)) -> do

    -- Activity bar
    let nd = taskGraph IM.! i
        x0 = max start (taskStart nd)
        x1 = min end (taskEnd nd)
        y0 = y * taskRowHgt
    when (x0 < x1) $ do
      setLineWidth (1/scaleValue)
      setSourceRGBAhex (if not bwMode then runningColour else black) 1.0
      draw_rectangle x0 y0
                     (x1-x0) taskBarHeight

    -- Left context
    let lnd = case taskParents nd of
          [] -> i
          ps -> maximumBy (comparing $ taskEnd . (taskGraph IM.!)) ps
        lx = max start $ taskEnd $ taskGraph IM.! lnd
    when (lx < x0) $ do
      setLineWidth 1
      setSourceRGBAhex grey 1.0
      draw_line (lx, y0 + taskBarHeight `div` 2)
                (x0, y0 + taskBarHeight `div` 2)
      case taskLayout IM.! lnd of
        (ly, Right lh) | lx > start && y >= ly && y < ly + lh -> do
          let ly0 = ly * taskRowHgt + taskBarHeight `div` 2
          setLineWidth (scaleValue)
          draw_line (lx, ly0)
                    (lx, y0 + taskBarHeight `div` 2)
        _other -> return ()

    -- Right context
    let rnd = case taskChilds nd of
          [] -> i
          cs -> minimumBy (comparing $ taskStart . (taskGraph IM.!)) cs
        rx = min end $ taskStart (taskGraph IM.! rnd)
    when (rx > x1) $ do
      setLineWidth 1
      setSourceRGBAhex grey 1.0
      draw_line (x1, y0 + taskBarHeight `div` 2)
                (rx, y0 + taskBarHeight `div` 2)
      case taskLayout IM.! rnd of
        (ry, Left rh) | rx < end && y >= ry && y < ry + rh -> do
          let ry0 = ry * taskRowHgt + taskBarHeight `div` 2
          setLineWidth (scaleValue)
          draw_line (rx, ry0)
                    (rx, y0 + taskBarHeight `div` 2)
        _other -> return ()

