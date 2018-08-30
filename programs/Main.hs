{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (void, forever)
import Control.Monad.IO.Class
import Control.Lens
import Graphics.Vty

import Model
import qualified Ivy as I

import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

import Control.Concurrent (threadDelay, forkIO)
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent.STM
import Numeric


data Tick = Tick

drawUI :: St -> [Widget ()]
drawUI st = [ui]
    where
        ui = withBorderStyle unicode $
            borderWithLabel
            (str $ getString st)
            $ rows
            where
                vals = getValues st
                minVal = if length vals == 0
                         then 0
                         else max (-1.0) (minimum vals)
                maxVal = if length vals == 0
                         then 1
                         else maximum vals
                rows = makeBarChart minVal maxVal vals

makeBarChart :: Double -> Double -> [Double] -> Widget ()
makeBarChart minVal maxVal vals =
    Widget Fixed Fixed $ do
        ctx <- getContext
        let height = ctx^.availHeightL
            drawableHeight = height - 1
            yAxis = drawLabelY minVal maxVal drawableHeight
            xAxis = drawLabelX
            figure = drawBox vals minVal maxVal
        render $ yAxis <+> (figure <=> xAxis)

-- draw the X axis, ideally with some time scale
drawLabelX :: Widget ()
drawLabelX = vLimit 1 $ fill '-'

-- draw y axis label, interleaved with '|'
drawLabelY :: Double -> Double -> Int -> Widget ()
drawLabelY minVal maxVal height = 
    if minVal == maxVal
    then vBox [str (showFFloat (Just 2) x " ") | x <- [fromIntegral height ::Double .. 1.0]]
    else vBox [drawMyValue i | i <- [0..n]]
        where
            n = height-1
            dt = (maxVal - minVal) / fromIntegral n
            drawMyValue i = if even i
                            then str " "
                            else str (showFFloat (Just 2) (maxVal - (fromIntegral i) *dt) " ")


-- drawBox get limits from [Double] (min, max)
-- and scale the internals accordingly
drawBox :: [Double] -> Double -> Double -> Widget ()
drawBox vals minVal maxVal =
    Widget Greedy Greedy $ do
        ctx <- getContext
        let width = ctx^.availWidthL
            height = ctx^.availHeightL
        render $ hBox [ drawColumn x minVal maxVal height | x <- (take width vals) ]


drawColumn :: Double -> Double -> Double -> Int -> Widget ()
drawColumn val minVal maxVal height =
    vBox $ topFiller : bottomFillers
                where
                    topFiller = hLimit 1 $ fill ' '
                    bottomFillers = replicate n (withAttr lineAttr (str "*"))
                    n = max 1 (round ( (val - minVal)/ (maxVal-minVal) * (fromIntegral (height)) ) )

appEvent :: St -> BrickEvent () Tick -> EventM () (Next St)
appEvent st (VtyEvent (EvKey k [])) =
    case k of
        KChar 'q'  -> halt st
        KEsc       -> halt st
        KChar '+'  -> do
            st' <- liftIO $ setTimer ( + 100000) st 
            continue st'
        KChar '-'  -> do
            st' <- liftIO $ setTimer (decreaseDelay 100000) st 
            continue st'
        _          -> continue st
appEvent st (AppEvent Tick) = do
    st' <- liftIO $ step st
    continue st'
appEvent st _ = continue st

decreaseDelay :: Int -> Int -> Int
decreaseDelay b a =  if (a - b) < 0
                     then 0
                     else a - b

lineAttr, emptyAttr :: AttrName
lineAttr = "lineAttr"
emptyAttr = "emptyAttr"

theMap :: AttrMap
theMap = attrMap defAttr
    [ (lineAttr,  green `on` green)
    , (emptyAttr,  white `on` black)
    ]

app :: App St Tick ()
app =
    App { appDraw = drawUI
        , appHandleEvent = appEvent
        , appAttrMap = const theMap
        , appChooseCursor = neverShowCursor
        , appStartEvent = return
        }

control_thread :: TVar Int -> BChan Tick -> IO ()
control_thread delay chan = forever $ do
    writeBChan chan Tick
    ms <- atomically $ readTVar delay
    threadDelay ms


main :: IO ()
main = do
  chan <- newBChan 10
  delay <- atomically $ newTVar 1000000
  source <- atomically $ newTVar 1.0
  void $ forkIO $ control_thread delay chan
  void $ forkIO $ I.ivyMain source
  st <- initialState delay source
  void $ customMain
    (mkVty defaultConfig) (Just chan) (app) (st)
