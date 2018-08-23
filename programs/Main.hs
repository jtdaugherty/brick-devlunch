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

data Tick = Tick

drawUI :: St -> [Widget ()]
drawUI st = [ui]
    where
        ui = withBorderStyle unicode $
            borderWithLabel
            (str $ getString st)
            $ withAttr lineAttr rows
            where
                rows = drawBox $ getNoiseValues st

drawBox :: [Int] -> Widget ()
--drawBox vals = hBox [ drawColumn v | v <- vals ]
drawBox = hBox . map drawColumn

drawColumn :: Int -> Widget ()
drawColumn n = vBox [ hLimit 1 $ fill ' ' , padBottom (Pad (n-1)) (str "X")]

appEvent :: TVar Int -> St -> BrickEvent () Tick -> EventM () (Next St)
appEvent delay st (VtyEvent (EvKey k [])) =
    case k of
        KChar 'q'  -> halt st
        KEsc       -> halt st
        KChar '+'  -> do
            liftIO $ atomically $ modifyTVar delay (+ 100000)
            continue st
        KChar '-'  -> do
            liftIO $ atomically $ modifyTVar delay ((-) 100000)
            continue st
        _          -> continue st
appEvent _ st (AppEvent Tick) = do
    st' <- liftIO $ step st
    continue st'
appEvent _ st _ = continue st

lineAttr, emptyAttr :: AttrName
lineAttr = "lineAttr"
emptyAttr = "emptyAttr"

theMap :: AttrMap
theMap = attrMap defAttr
    [ (lineAttr,  red `on` white)
    , (emptyAttr,  white `on` white)
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
  forkIO $ control_thread delay chan
  void $ customMain (mkVty defaultConfig) (Just chan) (app delay) initialState
