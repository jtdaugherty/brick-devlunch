{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>))
import Control.Monad (void, forever)
import Control.Monad.IO.Class
import Control.Lens
import Data.List (intersperse)
import Data.Monoid
import Graphics.Vty

import Model
import qualified Ivy as I

import Brick
import Brick.BChan
import Brick.Main
import Brick.Util
import Brick.AttrMap
import Brick.Widgets.Core
import Brick.Widgets.Center
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
                         else minimum vals
                maxVal = if length vals == 0
                         then 1
                         else maximum vals
                --rows = hBox [drawLabelY minVal maxVal, drawBox $ vals]
                rows = hBox [customWidget minVal maxVal vals]

customWidget :: Double -> Double -> [Double] -> Widget ()
customWidget minVal maxVal vals =
    Widget Fixed Fixed $ do
        ctx <- getContext
        let width = ctx^.availWidthL
        let height = ctx^.availHeightL
        let yAxis = drawLabelY minVal maxVal height
        let figure = drawBox vals minVal maxVal height width
        render $ hBox [yAxis, figure]

-- draw y axis label, interleaved with '|'
drawLabelY :: Double -> Double -> Int -> Widget ()
drawLabelY minVal maxVal height = 
    if minVal == maxVal
    then vBox [str (showFFloat (Just 2) x " ") | x <- [fromIntegral height .. 1.0]]
    else vBox [str (showFFloat (Just 2) x " ") | x <- [maxVal, (maxVal - dt) .. minVal]]
        where
            dt = (maxVal - minVal) / fromIntegral height


-- drawBox get limits from [Double] (min, max)
-- and scale the internals accordingly
drawBox :: [Double] -> Double -> Double -> Int -> Int -> Widget ()
drawBox vals minVal maxVal height width =
    hBox [ drawColumn x minVal maxVal height | x <- vals ]


drawColumn :: Double -> Double -> Double -> Int -> Widget ()
drawColumn val minVal maxVal height =
    vBox $ topFiller : bottomFillers
                where
                    topFiller = hLimit 1 $ fill ' '
                    bottomFillers = replicate n (withAttr lineAttr (str "*"))
                    n = round ( (val - minVal)/ (maxVal-minVal) * (fromIntegral height))

appEvent :: TVar Int -> St -> BrickEvent () Tick -> EventM () (Next St)
appEvent delay st (VtyEvent (EvKey k [])) =
    case k of
        KChar 'q'  -> halt st
        KEsc       -> halt st
        KChar '+'  -> do
            liftIO $ atomically $ modifyTVar delay ( + 100000)
            continue st
        KChar '-'  -> do
            liftIO $ atomically $ modifyTVar delay (decreaseDelay 100000)
            continue st
        _          -> continue st
appEvent _ st (AppEvent Tick) = do
    st' <- liftIO $ step st
    continue st'
appEvent _ st _ = continue st

decreaseDelay :: Int -> Int -> Int
decreaseDelay b a =  if (a - b) < 0
                     then 0
                     else a - b

lineAttr, emptyAttr :: AttrName
lineAttr = "lineAttr"
emptyAttr = "emptyAttr"

theMap :: AttrMap
theMap = attrMap defAttr
    [ (lineAttr,  red `on` red)
    , (emptyAttr,  white `on` black)
    ]

app :: TVar Int -> App St Tick ()
app delay =
    App { appDraw = drawUI
        , appHandleEvent = appEvent delay
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
  forkIO $ control_thread delay chan
  forkIO $ I.ivyMain source
  void $ customMain
    (mkVty defaultConfig) (Just chan) (app delay) (initialState delay source)
