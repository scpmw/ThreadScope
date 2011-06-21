module GUI.Timeline.HEC (
    renderHEC,
    renderSparks,
  ) where

import GUI.Timeline.Render.Constants

import Events.EventTree
import Events.SparkTree
import qualified Events.SparkCounters as SparkCounters
import Events.EventDuration
import GUI.Types
import GUI.Timeline.CairoDrawing
import GUI.ViewerColours

import Graphics.Rendering.Cairo
import qualified Graphics.Rendering.Cairo as C
import Graphics.UI.Gtk

import qualified GHC.RTS.Events as GHC
import GHC.RTS.Events hiding (Event, GCWork, GCIdle)

import Control.Monad

-- import Text.Printf

renderHEC :: Int -> ViewParameters
          -> Timestamp -> Timestamp -> (DurationTree, EventTree, SparkTree)
          -> Render ()
renderHEC cap params@ViewParameters{..} start end (dtree, etree, _stree) = do
  renderDurations cap params start end dtree
  when (scaleValue < detailThreshold) $
     case etree of
       EventTree ltime etime tree ->
           renderEvents cap params ltime etime start end tree

detailThreshold :: Double
detailThreshold = 3000

-------------------------------------------------------------------------------
-- hecView draws the trace for a single HEC

renderDurations :: Int -> ViewParameters
                -> Timestamp -> Timestamp -> DurationTree
                -> Render ()

renderDurations _ _ _ _ DurationTreeEmpty = return ()

renderDurations c params@ViewParameters{..} startPos endPos (DurationTreeLeaf e)
  | inView startPos endPos e = drawDuration c params e
  | otherwise                = return ()

renderDurations !c params@ViewParameters{..} !startPos !endPos
        (DurationSplit s splitTime e lhs rhs runAv gcAv)
  | startPos < splitTime && endPos >= splitTime &&
          (fromIntegral (e - s) / scaleValue) <= fromIntegral detail
  = -- View spans both left and right sub-tree.
    -- trace (printf "hecView (average): start:%d end:%d s:%d e:%d" startPos endPos s e) $
    drawAverageDuration c params s e runAv gcAv

  | otherwise
  = -- trace (printf "hecView: start:%d end:%d s:%d e:%d" startPos endPos s e) $
    do when (startPos < splitTime) $
         renderDurations c params startPos endPos lhs
       when (endPos >= splitTime) $
         renderDurations c params startPos endPos rhs

-------------------------------------------------------------------------------

renderSparks :: ViewParameters -> Timestamp -> Timestamp -> SparkTree
                -> Render ()
renderSparks ViewParameters{..} !start0 !end0 t = do
  let slice = round (fromIntegral spark_detail * scaleValue)
      -- round the start time down, and the end time up, to a slice boundary
      start = (start0 `div` slice) * slice
      end   = ((end0 + slice) `div` slice) * slice
      prof  = sparkProfile slice start end t
  drawSparks start end slice prof

spark_detail :: Int
spark_detail = 4 -- in pixels

spark_max, spark_per_pixel :: Double
-- Maximum value of s3 in current data. TODO: calculate
spark_max = 35000.0
-- Sparks per pixel for current data.
spark_per_pixel = spark_max / fromIntegral hecSparksHeight

drawSparks :: Timestamp -> Timestamp -> Timestamp
              -> [SparkCounters.SparkCounters]
              -> Render ()
drawSparks start end slice ts = do
  let gap  = 2 * spark_per_pixel  -- a gap for fill and to avoid brown colour
      s1 c = fromIntegral $ SparkCounters.sparksFizzled c
      f1 c = s1 c
      s2 c = fromIntegral $ s1 c + SparkCounters.sparksConverted c
      f2 c = gap + s2 c
      s3 c = fromIntegral $ s2 c + SparkCounters.sparksGCd c
      f3 c = gap + gap + s3 c
  addSparks (0.5, 0.5, 0.5) f1 start end slice ts
  addSparks (0, 1, 0) f2 start end slice ts
  addSparks (1, 0, 0) f3 start end slice ts

addSparks :: (Double, Double, Double)
             -> (SparkCounters.SparkCounters -> Double)
             -> Timestamp -> Timestamp -> Timestamp
             -> [SparkCounters.SparkCounters]
             -> Render ()
addSparks (cR, cG, cB) f start end slice ts = do
  case ts of
   [] -> return ()
   t:ts -> do
     -- liftIO $ printf "ts: %s\n" (show (map f (t:ts)))
     -- liftIO $ printf "off: %s\n" (show (map off (t:ts) :: [Double]))
     let dstart = fromIntegral start
         dend   = fromIntegral end
         dslice = fromIntegral slice
         dheight = fromIntegral hecSparksHeight

     newPath
     moveTo (dstart-dslice/2) (off t)
     zipWithM_ lineTo (tail [dstart-dslice/2, dstart+dslice/2 ..]) (map off ts)
     setSourceRGB cR cG cB
     -- setSourceRGBAhex black 1.0
     save
     identityMatrix
     setLineWidth 1
     strokePreserve
     restore
{-
     lineTo dend   dheight
     lineTo dstart dheight
     setSourceRGB cR cG cB
     fill
-}
  where
    off :: SparkCounters.SparkCounters -> Double
    off t = fromIntegral hecSparksHeight - f t / spark_per_pixel

-------------------------------------------------------------------------------

renderEvents :: Int -> ViewParameters
             -> Timestamp -- start time of this tree node
             -> Timestamp -- end   time of this tree node
             -> Timestamp -> Timestamp -> EventNode
             -> Render ()

renderEvents !c params@ViewParameters{..} !s !e !startPos !endPos
        (EventTreeLeaf es)
  = sequence_ [ drawEvent c params e
              | e <- es, let t = time e, t >= startPos && t < endPos ]
renderEvents !c params@ViewParameters{..} !s !e !startPos !endPos
        (EventTreeOne ev)
  | t >= startPos && t < endPos = drawEvent c params ev
  | otherwise = return ()
  where t = time ev

renderEvents !c params@ViewParameters{..} !s !e !startPos !endPos
        (EventSplit splitTime lhs rhs)
  | startPos < splitTime && endPos >= splitTime &&
        (fromIntegral (e - s) / scaleValue) <= fromIntegral detail
  = drawTooManyEvents c params s e

  | otherwise
  = do when (startPos < splitTime) $
         renderEvents c params s splitTime startPos endPos lhs
       when (endPos >= splitTime) $
         renderEvents c params splitTime e startPos endPos rhs

-------------------------------------------------------------------------------
-- An event is in view if it is not outside the view.

inView :: Timestamp -> Timestamp -> EventDuration -> Bool
inView viewStart viewEnd event
  = not (eStart > viewEnd || eEnd <= viewStart)
  where
    eStart = startTimeOf event
    eEnd   = endTimeOf event

-------------------------------------------------------------------------------

drawAverageDuration :: Int -> ViewParameters
                    -> Timestamp -> Timestamp -> Timestamp -> Timestamp
                    -> Render ()
drawAverageDuration c ViewParameters{..} startTime endTime runAv gcAv
  = do setSourceRGBAhex (if not bwMode then runningColour else black) 1.0
       when (runAv > 0) $
         draw_rectangle startTime hecBarOff -- x, y
                        (endTime - startTime)        -- w
                         hecBarHeight
       setSourceRGBAhex black 1.0
       --move_to (oxs + startTime, 0)
       --relMoveTo (4/scaleValue) 13
       --unscaledText scaleValue (show nrEvents)
       setSourceRGBAhex (if not bwMode then gcColour else black) gcRatio
       draw_rectangle startTime      -- x
                      (hecBarOff+hecBarHeight)      -- y
                      (endTime - startTime)           -- w
                      (hecBarHeight `div` 2)             -- h

    where
    duration = endTime - startTime
--    runRatio :: Double
--    runRatio = (fromIntegral runAv) / (fromIntegral duration)
    gcRatio :: Double
    gcRatio = (fromIntegral gcAv) / (fromIntegral duration)

-------------------------------------------------------------------------------

unscaledText :: String -> Render ()
unscaledText text
  = do m <- getMatrix
       identityMatrix
       textPath text
       C.fill
       setMatrix m

-------------------------------------------------------------------------------

textWidth :: Double -> String -> Render TextExtents
textWidth _scaleValue text
  = do m <- getMatrix
       identityMatrix
       tExtent <- textExtents text
       setMatrix m
       return tExtent

-------------------------------------------------------------------------------

drawDuration :: Int -> ViewParameters -> EventDuration -> Render ()

drawDuration c ViewParameters{..}
                (ThreadRun t s startTime endTime)
  = do setSourceRGBAhex (if not bwMode then runningColour else black) 1.0
       setLineWidth (1/scaleValue)
       draw_rectangle_opt False
                      startTime                  -- x
                      hecBarOff                  -- y
                      (endTime - startTime)      -- w
                      hecBarHeight               -- h
       -- Optionally label the bar with the threadID if there is room
       tExtent <- textWidth scaleValue tStr
       let tw = textExtentsWidth  tExtent
           th = textExtentsHeight tExtent
       when (tw + 6 < fromIntegral rectWidth)
         $ do setSourceRGBAhex labelTextColour 1.0
              move_to (fromIntegral startTime + truncate (4*scaleValue),
                       hecBarOff + (hecBarHeight + round th) `quot` 2)
              unscaledText tStr

        -- Optionally write the reason for the thread being stopped
        -- depending on the zoom value
       labelAt labelsMode endTime $
             show t ++ " " ++ showThreadStopStatus s
    where
    rectWidth = truncate (fromIntegral (endTime - startTime) / scaleValue) -- as pixels
    tStr = show t

drawDuration c ViewParameters{..} (GCStart startTime endTime)
  = gcBar (if bwMode then black else gcStartColour) startTime endTime

drawDuration c ViewParameters{..} (GCWork startTime endTime)
  = gcBar (if bwMode then black else gcWorkColour) startTime endTime

drawDuration c ViewParameters{..} (GCIdle startTime endTime)
  = gcBar (if bwMode then black else gcIdleColour) startTime endTime

drawDuration c ViewParameters{..} (GCEnd startTime endTime)
  = gcBar (if bwMode then black else gcEndColour) startTime endTime

gcBar :: Color -> Timestamp -> Timestamp -> Render ()
gcBar col !startTime !endTime
  = do setSourceRGBAhex col 1.0
       draw_rectangle_opt False
                      startTime                      -- x
                      (hecBarOff+hecBarHeight)       -- y
                      (endTime - startTime)          -- w
                      (hecBarHeight `div` 2)         -- h

labelAt :: Bool -> Timestamp -> String -> Render ()
labelAt labelsMode t str
  | not labelsMode = return ()
  | otherwise = do
       setSourceRGB 0.0 0.0 0.0
       move_to (t, hecBarOff+hecBarHeight+12)
       save
       identityMatrix
       rotate (pi/4)
       textPath str
       C.fill
       restore

drawEvent :: Int -> ViewParameters -> GHC.Event -> Render ()
drawEvent c params@ViewParameters{..} event
  = case spec event of
      CreateThread{}   -> renderInstantEvent params event createThreadColour
      RunSpark{}       -> renderInstantEvent params event runSparkColour
      StealSpark{}     -> renderInstantEvent params event stealSparkColour
      ThreadRunnable{} -> renderInstantEvent params event threadRunnableColour
      RequestSeqGC{}   -> renderInstantEvent params event seqGCReqColour
      RequestParGC{}   -> renderInstantEvent params event parGCReqColour
      MigrateThread{}  -> renderInstantEvent params event migrateThreadColour
      WakeupThread{}   -> renderInstantEvent params event threadRunnableColour
      Shutdown{}       -> renderInstantEvent params event shutdownColour

      RunThread{}  -> return ()
      StopThread{} -> return ()
      StartGC{}    -> return ()

      _ -> return ()

renderInstantEvent :: ViewParameters -> GHC.Event -> Color -> Render ()
renderInstantEvent ViewParameters{..} event color = do
     setSourceRGBAhex color 1.0
     setLineWidth (3 * scaleValue)
     let t = time event
     draw_line (t, hecBarOff-4) (t, hecBarOff+hecBarHeight+4)
     labelAt labelsMode t $ showEventTypeSpecificInfo (spec event)


drawTooManyEvents :: Int -> ViewParameters -> Timestamp -> Timestamp
                  -> Render ()
drawTooManyEvents c params@ViewParameters{..} start end = do
     return ()
--     setSourceRGBAhex grey 1.0
--     setLineWidth (3 * scaleValue)
--     draw_rectangle start (hecBarOff-4) (end - start) 4
--     draw_rectangle start (hecBarOff+hecBarHeight) (end - start) 4

-------------------------------------------------------------------------------
