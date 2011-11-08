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
renderVRulers :: Double -> Timestamp -> Timestamp -> Int -> XScaleMode
                 -> Render()
renderVRulers scaleValue startPos endPos height xScaleMode = do
  let timestampFor100Pixels = truncate (100 * scaleValue)
      snappedTickDuration :: Timestamp
      snappedTickDuration =
        10 ^ truncate (logBase 10 (fromIntegral timestampFor100Pixels) :: Double)
      tickWidthInPixels :: Double
      tickWidthInPixels = fromIntegral snappedTickDuration / scaleValue
      firstTick :: Timestamp
      firstTick = snappedTickDuration * (startPos `div` snappedTickDuration)
  setSourceRGBAhex black 0.15
  setLineWidth scaleValue
  case xScaleMode of
    XScaleTime ->
      drawVRulers tickWidthInPixels scaleValue
        (fromIntegral $ firstTick + snappedTickDuration)
        (fromIntegral snappedTickDuration) endPos height
        (1 + fromIntegral (startPos `div` snappedTickDuration))
    XScaleLog _ dx ->
      drawVRulers 1e1000 1 dx dx endPos height 1

-- | Render a single vertical ruler and then recurse.
drawVRulers :: Double -> Double -> Double -> Double
               -> Timestamp -> Int -> Int -> Render ()
drawVRulers tickWidthInPixels scaleValue pos incr endPos height i =
  if floor pos <= endPos then do
    when (atMajorTick || atMidTick || tickWidthInPixels > 30) $ do
      draw_line (round pos, 0) (round pos, height)
    drawVRulers
      tickWidthInPixels scaleValue (pos + incr) incr endPos height (i + 1)
  else
    return ()
  where
    atMidTick = i `mod` 5 == 0
    atMajorTick = i `mod` 10 == 0


-- | Render the X scale, based on view parameters and hecs.
renderXScaleArea :: ViewParameters -> HECs -> Render ()
renderXScaleArea ViewParameters{width, scaleValue, hadjValue, xScaleAreaHeight}
                 hecs =
  let lastTx = hecLastEventTime hecs
      off y = xScaleAreaHeight - y
  in renderXScale scaleValue hadjValue lastTx width off XScaleTime


data XScaleMode = XScaleTime | XScaleLog Double Double deriving Eq

-- | Render the X (vertical) scale: render X axis and call ticks rendering.
-- TODO: refactor common parts with renderVRulers, in particlar to expose
-- that ruler positions match tick positions.
renderXScale :: Double -> Double -> Timestamp -> Int
                -> (Int -> Int) -> XScaleMode
                -> Render ()
renderXScale scaleValue hadjValue lastTx width off xScaleMode = do
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
-- setLineCap LineCapRound -- TODO: breaks rendering currently (see BrokenX.png)
  setLineWidth 1.0  -- TODO: it's not really 1 pixel, due to the scale
  -- TODO: snap to pixels, currently looks semi-transparent
  draw_line (startPos, off 16) (endPos, off 16)
  let tFor100Pixels = truncate (100 * scaleValue)
      snappedTickDuration :: Timestamp
      snappedTickDuration =
        10 ^ truncate (logBase 10 (fromIntegral tFor100Pixels) :: Double)
      tickWidthInPixels :: Double
      tickWidthInPixels = fromIntegral snappedTickDuration / scaleValue
      firstTick :: Timestamp
      firstTick = snappedTickDuration * (startPos `div` snappedTickDuration)
  setLineWidth scaleValue  -- TODO: should be 0.5 pixels (when we rewrite stuff)
  case xScaleMode of
    XScaleTime ->
      drawXTicks tickWidthInPixels scaleValue (fromIntegral firstTick)
        (fromIntegral snappedTickDuration) endPos off xScaleMode
        (fromIntegral (startPos `div` snappedTickDuration))
    XScaleLog _ segmentWidth ->
      drawXTicks 1e1000 1 0 segmentWidth endPos off xScaleMode 0
  restore

-- | Render a single X scale tick and then recurse.
drawXTicks :: Double -> Double -> Double -> Double -> Timestamp
              -> (Int -> Int) -> XScaleMode -> Int
              -> Render ()
drawXTicks tickWidthInPixels scaleValue pos incr endPos off xScaleMode i =
  if floor pos <= endPos then do
    -- TODO: snap to pixels, currently looks semi-transparent
    draw_line (x1, off 16) (x1, off (16 - tickLength))
    when (atMajorTick || atMidTick || tickWidthInPixels > 30) $ do
      tExtent <- textExtents tickTimeText
      move_to textPos
      m <- getMatrix
      identityMatrix
      (fourPixels, _) <- deviceToUserDistance 4 0
      when (floor (pos + incr) <= endPos
            && (isWideEnough tExtent fourPixels || atMajorTick)) $
        showText tickTimeText
      setMatrix m
    drawXTicks
      tickWidthInPixels scaleValue (pos + incr) incr endPos off xScaleMode (i+1)
  else
    return ()
  where
    atMidTick = xScaleMode == XScaleTime && i `mod` 5 == 0
    atMajorTick = xScaleMode == XScaleTime && i `mod` 10 == 0
    textPos =
      if xScaleMode == XScaleTime
      then (x1 - truncate (scaleValue * 2), off 26)
      else (x1 + ceiling (scaleValue * 2), tickLength + 13)
    tickLength | atMajorTick = 16
               | atMidTick = 12
               | otherwise = 8
    posTime = case xScaleMode of
                XScaleTime ->  round pos
                XScaleLog minX _ -> round $ 2 ** (minX + pos / incr)
    tickTimeText = showMultiTime posTime
    width = if atMidTick then 5 * tickWidthInPixels
            else tickWidthInPixels
    isWideEnough tExtent fourPixels =
      textExtentsWidth tExtent + fourPixels < width
    -- We cheat at pos 0, to avoid half covering the tick by the grey label area.
    lineWidth = scaleValue
    x1 = round $ if pos == 0 then lineWidth else pos

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
      incr = fromIntegral hecSparksHeight / 10
  -- dashed lines across the graphs
  setSourceRGBAhex black 0.3
  save
  forM_ [0, 5] $ \h -> do
    let y = h * incr
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
  save
  newPath
  moveTo xoffset yoffset
  lineTo xoffset (yoffset + fromIntegral hecSparksHeight)
  setSourceRGBAhex black 1.0
  setLineCap LineCapRound
  setLineWidth 1.0  -- TODO: it's not really 1 pixel, due to the scale
  stroke
  selectFontFace "sans serif" FontSlantNormal FontWeightNormal
  setFontSize 12
  scale scaleValue 1.0
  setLineWidth 0.5  -- TODO: it's not really 0.5 pixels, due to the scale
  drawYTicks maxS 0 incr xoffset yoffset 0
  restore

-- | Render a single Y scale tick and then recurse.
drawYTicks :: Double -> Double -> Double -> Double -> Double -> Int -> Render ()
drawYTicks maxS pos incr xoffset yoffset i =
  if i <= 10 then do
    -- TODO: snap to pixels, currently looks semi-transparent
    moveTo xoffset (yoffset + majorTick - pos)
    lineTo (xoffset + tickLength) (yoffset + majorTick - pos)
    stroke
    when (atMajorTick || atMidTick) $ do
      tExtent <- textExtents tickText
      (fewPixels, _) <- deviceToUserDistance 8 0
      moveTo (xoffset - textExtentsWidth tExtent - fewPixels)
             (yoffset + majorTick - pos + fewPixels / 2)
      when (atMidTick || atMajorTick) $
        showText tickText
    drawYTicks maxS (pos + incr) incr xoffset yoffset (i + 1)
  else
    return ()
  where
    atMidTick = i `mod` 5 == 0
    atMajorTick = i `mod` 10 == 0
    majorTick = 10 * incr
    tickText = reformatV (fromIntegral i * maxS / 10)
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
