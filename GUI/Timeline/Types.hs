module GUI.Timeline.Types (
    TimelineState(..),
    TimeSelection(..),
    TimelineHint(..)
 ) where


import GUI.Types

import Events.TimeTree (TimeTree)
import Events.Debug (SampleWeight)

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Data.IORef

-----------------------------------------------------------------------------

data TimelineState = TimelineState {
       timelineDrawingArea :: DrawingArea,
       timelineYScaleArea  :: DrawingArea,
       timelineXScaleArea  :: DrawingArea,
       timelineAdj         :: Adjustment,
       timelineVAdj        :: Adjustment,

       timelinePrevView    :: IORef (Maybe (ViewParameters, Surface)),

       -- This scale value is used to map a micro-second value to a pixel unit.
       -- To convert a timestamp value to a pixel value, multiply it by scale.
       -- To convert a pixel value to a micro-second value, divide it by scale.
       scaleIORef          :: IORef Double,

       -- Maximal number of sparks/slice measured after every zoom to fit.
       maxSpkIORef         :: IORef Double,

       -- | Hint data: Something that we should highlight about what
       -- the timeline is showing to provide context
       hintIORef           :: IORef TimelineHint
     }


data TimeSelection = PointSelection Timestamp
                   | RangeSelection Timestamp Timestamp

data TimelineHint = NoHint
                  | ActivityHint (TimeTree SampleWeight) (TimeTree SampleWeight)

-----------------------------------------------------------------------------
