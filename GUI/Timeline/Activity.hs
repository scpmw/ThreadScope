module GUI.Timeline.Activity (
      renderActivity
  ) where

import GUI.Timeline.Render.Constants
import GUI.Timeline.Types

import Events.HECs
import Events.EventTree
import Events.TimeTree
import Events.Debug (SampleWeight(..))
import GUI.Types
import GUI.ViewerColours

import Graphics.Rendering.Cairo

import Control.Monad
import Data.List

-- ToDo:
--  - we average over the slice, but the point is drawn at the beginning
--    of the slice rather than in the middle.

-----------------------------------------------------------------------------

renderActivity :: ViewParameters -> HECs -> TimelineHint -> Timestamp -> Timestamp
               -> Render ()

renderActivity ViewParameters{..} hecs hint start0 end0 = do
  let
      slice = ceiling (fromIntegral activity_detail * scaleValue)

      -- round the start time down, and the end time up, to a slice boundary
      start = (start0 `div` slice) * slice
      end   = ((end0 + slice) `div` slice) * slice

      hec_profs  = map (actProfile slice start end)
                     (map (\ (t, _, _) -> t) (hecTrees hecs))
      total_prof = map sum (transpose hec_profs)

      hint_prof = case hint of
        ActivityHint hintTree totalTree
               -> hintProfile slice start end hintTree totalTree
        _other -> []

--  liftIO $ printf "%s\n" (show (map length hec_profs))
--  liftIO $ printf "%s\n" (show (map (take 20) hec_profs))
  drawActivity hecs start end slice total_prof hint_prof
               (if not bwMode then runningColour else black)

activity_detail :: Int
activity_detail = 4 -- in pixels

-- for each timeslice in the period (and one outside at each end), the
-- amount of time spent in the mutator during that period.
actProfile :: Timestamp -> Timestamp -> Timestamp -> DurationTree -> [Timestamp]
actProfile slice start end t
  | start < slice  = 0 : mkProf start t
  | otherwise      = mkProf (start - slice) t
  where
    mkProf start' = map (runTimeOf . timeTreeVal) .
                    sliceTimeTree clamp (start', end+slice) slice
    -- The clamp function will only ever get used on leaf nodes,
    -- therefore we can assume that all the cost happens at the
    -- *beginning* of the old interval. We build a new node
    -- accordingly. Note that we drop GC times when clamping nodes, as
    -- we are only interested in mutator cost.
    clamp (oldl, _) (newl, newr) v =
      let x = oldl + runTimeOf v
          time | x >= newl  = (x - newl) `min` (newr - newl)
               | otherwise  = 0
      in OverviewNode time 0

-- for each time slice, the percentage of activity that should be hinted at
hintProfile :: Timestamp -> Timestamp -> Timestamp
              -> TimeTree SampleWeight -> TimeTree SampleWeight -> [Double]
hintProfile slice start end hintTree totalTree
  | start < slice  = 0 : mkProf start
  | otherwise      = mkProf (start - slice)
  where
    mkProf start' = zipWith calcHint (slice_f start' hintTree) (slice_f start' totalTree)
    slice_f start' = sliceTimeTree clamp (start', end+slice) slice
    -- In contrast to above, we assume that weights are distributed
    -- uniformely over the period.
    clamp (oldl, oldr) (newl, newr) (SampleWeight w) =
      let old_len = oldr - oldl
          new_len = newr - newl
      in SampleWeight $ w * new_len `div` old_len

    calcHint hintSTree totalSTree =
      let hint = unSampleWeight $ timeTreeVal hintSTree
          total = unSampleWeight $ timeTreeVal totalSTree
      in case total of
        0 -> 0
        _ -> fromIntegral hint / fromIntegral total

drawActivity :: HECs -> Timestamp -> Timestamp -> Timestamp -> [Timestamp] -> [Double]
             -> Color
             -> Render ()
drawActivity hecs start end slice ts hints color = do

     let dstart = fromIntegral start
         dend   = fromIntegral end
         dslice = fromIntegral slice
         dheight = fromIntegral activityGraphHeight

-- funky gradients don't seem to work:
--     withLinearPattern 0 0 0 dheight $ \pattern -> do
--        patternAddColorStopRGB pattern 0   0.8 0.8 0.8
--        patternAddColorStopRGB pattern 1.0 1.0 1.0 1.0
--        rectangle dstart 0 dend dheight
--        setSource pattern
--        fill

     -- Path for a profile
     let off t = fromIntegral activityGraphHeight -
            fromIntegral (t * fromIntegral activityGraphHeight) /
            fromIntegral (fromIntegral (hecCount hecs) * slice)
         graphPath [] = return ()
         graphPath (t:ts) = do
           newPath
           moveTo (dstart-dslice/2) (off t)
           zipWithM_ lineTo (tail [dstart-dslice/2, dstart+dslice/2 ..]) (map off ts)
           lineTo dend   dheight
           lineTo dstart dheight

     -- Set up for drawing graph
     setSourceRGBAhex black 1.0
     setLineWidth 1
     strokePreserve
     setSourceRGBAhex color 1.0

     -- First the actual activity graph
     graphPath ts
     fill

     -- Then draw hint over it (if any)
     setSourceRGBAhex green 1.0
     let calcHint t h = round (fromIntegral t * h)
     graphPath $ zipWith calcHint ts hints
     fill

-- funky gradients don't seem to work:
--      save
--      withLinearPattern 0 0 0 dheight $ \pattern -> do
--        patternAddColorStopRGB pattern 0   0   1.0 0
--        patternAddColorStopRGB pattern 1.0 1.0 1.0 1.0
--        setSource pattern
-- --       identityMatrix
-- --       setFillRule FillRuleEvenOdd
--        fillPreserve
--      restore

     save
     forM_ [0 .. hecCount hecs - 1] $ \h -> do
       let y = fromIntegral (floor (fromIntegral h * dheight / fromIntegral (hecCount hecs))) - 0.5
       setSourceRGBAhex black 0.3
       moveTo dstart y
       lineTo dend y
       dashedLine1
     restore

 where

-- | Draw a dashed line along the current path.
dashedLine1 :: Render ()
dashedLine1 = do
  save
  identityMatrix
  let dash = fromIntegral ox
  setDash [dash, dash] 0.0
  setLineWidth 1
  stroke
  restore
