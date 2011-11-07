{-# LANGUAGE CPP #-}
module GUI.Timeline.Ticks (
    renderVRulers,
    XScaleMode(..),
    renderXScaleArea,
    renderXScale,
    renderHRulers,
    renderYScale,
    mu,
    deZero,
    dashedLine1,
  ) where

import Events.HECs
import GUI.Types
import GUI.Timeline.Render.Constants
import GUI.Timeline.CairoDrawing
import GUI.ViewerColours

import Graphics.Rendering.Cairo
import Control.Monad
import Text.Printf

-- Minor, semi-major and major ticks are drawn and the absolute period of
-- the ticks is determined by the zoom level.
-- There are ten minor ticks to a major tick and a semi-major tick
-- occurs half way through a major tick (overlapping the corresponding
-- minor tick).
-- The timestamp values are in micro-seconds (1e-6) i.e.
-- a timestamp value of 1000000 represents 1s. The position on the drawing
-- canvas is in milliseconds (ms) (1e-3).
-- scaleValue is used to divide a timestamp value to yield a pixel value.
-- NOTE: the code below will crash if the timestampFor100Pixels is 0.
-- The zoom factor should be controlled to ensure that this never happens.

-- | Render vertical rulers (solid translucent lines), matching scale ticks.
renderVRulers :: Timestamp -> Timestamp -> Double -> Int -> Maybe Double
                 -> Render()
renderVRulers startPos endPos scaleValue height tick = do
  setSourceRGBAhex black 0.15
  let timestampFor100Pixels = truncate (100 * scaleValue)  -- micro-second time for 100 ps
      snappedTickDuration :: Timestamp
      snappedTickDuration =
        10 ^ truncate (logBase 10 (fromIntegral timestampFor100Pixels) :: Double)
      tickWidthInPixels :: Int
      tickWidthInPixels =
        truncate ((fromIntegral snappedTickDuration) / scaleValue)
      firstTick :: Timestamp
      firstTick = snappedTickDuration * (startPos `div` snappedTickDuration)
  setLineWidth scaleValue
  case tick of
    Nothing ->
      drawVRulers (fromIntegral tickWidthInPixels) height scaleValue
        (fromIntegral firstTick) (fromIntegral snappedTickDuration) endPos
    Just dx ->
      drawVRulers (dx / scaleValue) height scaleValue dx dx endPos

-- | Render a single vertical ruler and then recurse.
drawVRulers :: Double -> Int -> Double -> Double -> Double
               -> Timestamp -> Render ()
drawVRulers tickWidthInPixels height scaleValue pos incr endPos =
  if floor pos <= endPos then do
    when (atMajorTick || atMidTick || tickWidthInPixels > 30) $ do
      draw_line (x1, 0) (x1, height)
      drawVRulers
        tickWidthInPixels height scaleValue (pos + incr) incr endPos
  else
    return ()
  where
    midTick = 5 * round incr
    atMidTick = round pos `mod` midTick == 0
    majorTick = 10 * round incr
    atMajorTick = round pos `mod` majorTick == 0
    -- We cheat at pos 0, to avoid half covering the tick by the grey label area.
    lineWidth = scaleValue
    x1 = round $ if pos == 0 then lineWidth else pos


-- | Render the X scale, based on view parameters and hecs.
renderXScaleArea :: ViewParameters -> HECs -> Render ()
renderXScaleArea ViewParameters{width, scaleValue, hadjValue, xScaleAreaHeight}
                 hecs =
  let lastTx = hecLastEventTime hecs
      off y = xScaleAreaHeight - y
  in renderXScale scaleValue hadjValue width lastTx off XScaleTime


data XScaleMode = XScaleTime | XScaleLog Double Double deriving Eq

-- | Render the X (vertical) scale: render X axis and call ticks rendering.
-- TODO: refactor common parts with renderVRulers, in particlar to expose
-- that ruler positions match tick positions.
renderXScale :: Double -> Double -> Int -> Timestamp
                -> (Int -> Int) -> XScaleMode
                -> Render ()
renderXScale scaleValue hadjValue width lastTx off xScaleMode = do
  let scale_width = fromIntegral width * scaleValue
      startPos :: Timestamp
      startPos = truncate hadjValue
      endPos :: Timestamp
      endPos = ceiling $ minimum [hadjValue + scale_width, fromIntegral lastTx]
  save
  scale (1/scaleValue) 1.0
  translate (-hadjValue) 0
  selectFontFace "sans serif" FontSlantNormal FontWeightNormal
  setFontSize 12
  setSourceRGBAhex black 1.0
  setLineWidth 1.0
  draw_line (startPos, off 16) (endPos, off 16)
  let tFor100Pixels = truncate (100 * scaleValue)  -- micro-sec time for 100 pxs
      snappedTickDuration :: Timestamp
      snappedTickDuration =
        10 ^ truncate (logBase 10 (fromIntegral tFor100Pixels) :: Double)
      tickWidthInPixels :: Int
      tickWidthInPixels =
        truncate ((fromIntegral snappedTickDuration) / scaleValue)
      firstTick :: Timestamp
      firstTick = snappedTickDuration * (startPos `div` snappedTickDuration)
  setLineWidth scaleValue
  -- TODO: simplify the conversions from Double to Timestamp and back again,
  -- perhaps when eliminating scale from Cairo drawing
  case xScaleMode of
    XScaleTime ->
      drawXTicks tickWidthInPixels scaleValue (fromIntegral firstTick) (fromIntegral snappedTickDuration) endPos off xScaleMode
    XScaleLog _minX segmentWidth ->
      drawXTicks tickWidthInPixels 1 0 segmentWidth endPos off xScaleMode
  restore

-- | Render a single X scale tick and then recurse.
drawXTicks :: Int -> Double -> Double -> Double -> Timestamp
              -> (Int -> Int) -> XScaleMode
              -> Render ()
drawXTicks tickWidthInPixels scaleValue pos incr endPos off xScaleMode =
  if floor pos <= endPos then do
    draw_line (x1, off 16) (x1, off (16 - tickLength))
    when (xScaleMode == XScaleTime
          || atMajorTick || atMidTick || tickWidthInPixels > 30) $ do
      tExtent <- textExtents tickTimeText
      move_to testPos
      m <- getMatrix
      identityMatrix
      (fourPixels, _) <- deviceToUserDistance 4 0
      when (floor (pos + incr) <= endPos
            && (isWideEnough tExtent fourPixels || atMajorTick)) $
        showText tickTimeText
      setMatrix m
    drawXTicks
      tickWidthInPixels scaleValue (pos + incr) incr endPos off xScaleMode
  else
    return ()
  where
    testPos =
      if xScaleMode == XScaleTime
      then (x1 - truncate (scaleValue * 2), off 26)
      else (x1 + ceiling (scaleValue * 2), tickLength + 13)
    posTime = case xScaleMode of
                XScaleTime ->  round pos
                XScaleLog minX _ ->
                  round $ 2 ** (minX + pos / incr)
    tickTimeText = showMultiTime posTime
    width = if atMidTick then 5 * tickWidthInPixels
            else tickWidthInPixels
    isWideEnough tExtent fourPixels =
      textExtentsWidth tExtent + fourPixels < fromIntegral width
    midTick = 5 * (round incr)
    atMidTick = xScaleMode == XScaleTime && (round pos) `mod` midTick == 0
    majorTick = 10 * (round incr)
    atMajorTick = xScaleMode == XScaleTime && (round pos) `mod` majorTick == 0
    -- We cheat at pos 0, to avoid half covering the tick by the grey label area.
    lineWidth = scaleValue
    x1 = round $ if pos == 0 then lineWidth else pos
    tickLength | atMajorTick = 16
               | atMidTick = 12
               | otherwise = 8

-- | Display the micro-second time unit with an appropriate suffix
-- depending on the actual time value.
-- For times < 1e-6 the time is shown in micro-seconds.
-- For times >= 1e-6 and < 0.1 seconds the time is shown in ms
-- For times >= 0.5 seconds the time is shown in seconds
showMultiTime :: Timestamp -> String
showMultiTime pos =
  if pos == 0 then "0s"
  else if pos < 1000 then -- Show time as micro-seconds for times < 1e-6
         reformatMS posf ++ (mu ++ "s")  -- microsecond (1e-6s).
       else if pos < 100000 then -- Show miliseonds for time < 0.1s
              reformatMS (posf / 1000) ++ "ms" -- miliseconds 1e-3
            else -- Show time in seconds
              reformatMS (posf / 1000000) ++ "s"
  where
    posf :: Double
    posf = fromIntegral pos
    reformatMS :: Num a => a -> String
    reformatMS pos = deZero (show pos)

-------------------------------------------------------------------------------

-- | Render horizontal rulers (dashed translucent lines),
-- matching scale ticks (visible in the common @incr@ value and starting at 0).
renderHRulers :: Int -> Timestamp -> Timestamp -> Render ()
renderHRulers hecSparksHeight start end = do
  let dstart = fromIntegral start
      dend = fromIntegral end
      incr = hecSparksHeight `div` 10
  -- dashed lines across the graphs
  setSourceRGBAhex black 0.3
  save
  forM_ [0, 5] $ \h -> do
    let y = fromIntegral $ h * incr
    moveTo dstart y
    lineTo dend y
    dashedLine1
  restore

-- | Render one of the Y (horizontal) scales: render the Y axis
-- and call ticks rendering.
renderYScale :: Int -> Double -> Double -> Double -> Double -> Render ()
renderYScale hecSparksHeight scaleValue maxSpark xoffset yoffset = do
  let -- This is slightly off (by 1% at most), but often avoids decimal dot:
      maxS = if maxSpark < 100
             then maxSpark  -- too small, would be visible on screen
             else fromIntegral (2 * (ceiling maxSpark ` div` 2))
      incr = fromIntegral hecSparksHeight / 10

  newPath
  moveTo xoffset yoffset
  lineTo xoffset (yoffset + fromIntegral hecSparksHeight)
  setSourceRGBAhex black 1.0
  stroke

  selectFontFace "sans serif" FontSlantNormal FontWeightNormal
  setFontSize 12
  setSourceRGBAhex black 1.0
  save
  scale scaleValue 1.0
  setLineWidth 0.5
  let yoff = truncate yoffset
      xoff = truncate xoffset
  drawYTicks maxS 0 incr xoff yoff
  restore

-- | Render a single Y scale tick and then recurse.
drawYTicks :: Double -> Double -> Double -> Int -> Int -> Render ()
drawYTicks maxS pos incr xoffset yoffset =
  if floor pos <= ceiling majorTick then do
    draw_line (xoffset             , yoffset + ceiling (majorTick - pos))
              (xoffset + tickLength, yoffset + ceiling (majorTick - pos))
    when (atMajorTick || atMidTick) $ do
      tExtent <- textExtents tickText
      (fewPixels, _) <- deviceToUserDistance 8 0
      move_to (xoffset - ceiling (textExtentsWidth tExtent + fewPixels),
               yoffset + ceiling (majorTick - pos + fewPixels / 2))
      when (atMidTick || atMajorTick) $
        showText tickText
    drawYTicks maxS (pos + incr) incr xoffset yoffset
  else
    return ()
  where
    tickText = reformatV (fromIntegral interations * maxS / 10)
    interations = round (pos / incr)
    atMidTick = interations `mod` 5 == 0
    majorTick = 10 * incr
    atMajorTick = interations `mod` 10 == 0
    tickLength | atMajorTick = 13
               | atMidTick   = 10
               | otherwise   = 6
    reformatV :: Double -> String
    reformatV v = deZero (printf "%.2f" v)

-------------------------------------------------------------------------------

-- | The 'micro' symbol.
mu :: String
#if MIN_VERSION_cairo(0,12,0) && !MIN_VERSION_cairo(0,12,1)
-- this version of cairo doesn't handle Unicode properly.
-- Thus, we do the encoding by hand:
mu = "\194\181"
#else
-- Haskell cairo bindings 0.12.1 have proper Unicode support
mu = "\x00b5"
#endif

-- | Remove all meaningless trailing zeroes.
deZero :: String -> String
deZero s
  | '.' `elem` s =
    reverse . dropWhile (=='.') . dropWhile (=='0') . reverse $ s
  | otherwise = s

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
