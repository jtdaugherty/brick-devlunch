{-# LANGUAGE OverloadedStrings #-}
module Main where
{-- For book keeping:
    To Exit:
    `import System.Exit`
    and call:
    `exitWith ExitSuccess`

    To print message into stderr:
    `import System.IO (hPutStrLn, stderr)`
    and then:
    `hPutStrLn stderr $ "HELLO"`
    and run the program with:
    `$BINARY_NAME 2>out.txt`
    to redirect stderr to out.txt
--}

import qualified Options.Applicative as O
import Data.Semigroup ((<>))

import Control.Monad (void, forever)
import Control.Monad.IO.Class
import Control.Lens
import Graphics.Vty

import Model

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
                period = getPeriod st
                minVal = if length vals == 0
                         then 0
                         else max (-1.0) (minimum vals)
                maxVal = if length vals == 0
                         then 1
                         else maximum vals
                rows = makeBarChart minVal maxVal vals period

makeBarChart :: Double -> Double -> [Double] -> Double -> Widget ()
makeBarChart minVal maxVal vals period =
    Widget Fixed Fixed $ do
        ctx <- getContext
        let height = ctx^.availHeightL
            drawableHeight = height - 1
            yAxis = drawLabelY minVal maxVal drawableHeight
            figure = drawBox vals minVal maxVal period
        render $ yAxis <+> figure

-- draw the X axis, ideally with some time scale
drawLabelX :: Int -> Double -> Widget ()
drawLabelX m period = str $ labelX m
    where
        labelX :: Int -> String
        labelX 0 = ""
        labelX 1 = "1"
        labelX n | n `mod` 10 /= 0 = labelX (n-1) ++ "-"
                 | otherwise = labelX (n- (length label)) ++ label
                    where
                        label = show (round (nf * period))
                        nf = (fromIntegral n :: Double)

-- draw y axis label, interleaved with '|'
drawLabelY :: Double -> Double -> Int -> Widget ()
drawLabelY minVal maxVal height = 
    if minVal == maxVal
    then vBox 
        [str (showFFloat (Just 2) x " ") |
        x <- [fromIntegral height ::Double .. 1.0]]
    else vBox [drawMyValue i | i <- [0..n]]
        where
            n = height-1
            dt = (maxVal - minVal) / fromIntegral n
            drawMyValue i = if even i
                            then str " "
                            else str (showFFloat (Just 2) 
                                    (maxVal - (fromIntegral i) *dt) " ")

-- drawBox get limits from [Double] (min, max)
-- and scale the internals accordingly
drawBox :: [Double] -> Double -> Double -> Double -> Widget ()
drawBox vals minVal maxVal period =
    Widget Greedy Greedy $ do
        ctx <- getContext
        let width = ctx^.availWidthL
            height = ctx^.availHeightL
            xAxis = drawLabelX width period
        render $ (hBox 
            [ drawColumn x minVal maxVal height | x <- (take width vals) ])
            <=> xAxis

drawColumn :: Double -> Double -> Double -> Int -> Widget ()
drawColumn val minVal maxVal height =
    vBox $ topFiller : bottomFillers
                where
                    topFiller = hLimit 1 $ fill ' '
                    bottomFillers = replicate n (withAttr lineAttr (str "*"))
                    n = max 1
                        (round (
                            (val - minVal)/ (maxVal-minVal)
                            * (fromIntegral (height))
                        ))

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
    args <- O.execParser opts
    chan <- newBChan 10
    delayVar <- atomically $ newTVar (round (period args) * 1000000)
    void $ forkIO $ control_thread delayVar chan
    st <- initializeState delayVar (regexpr args) (name args) (field args)
    void $ customMain
        (mkVty defaultConfig) (Just chan) (app) (st)
        where
            opts = O.info (appOptions O.<**> O.helper)
                ( O.fullDesc
                <> O.progDesc descString
                <> O.header "Haskell Ivy plotter" )

data AppOptions = AppOptions
  { regexpr :: String
  , name    :: String
  , period  :: Double
  , field   :: Int}

appOptions :: O.Parser AppOptions
appOptions = AppOptions
      <$> O.strOption
        ( O.long "rexepr"
        <> O.short 'r'
        <> O.metavar "EXPR"
        <> O.help "Regular expression for Ivy bus binding" )
      <*> O.strOption
        ( O.long "name"
        <> O.short 'n'
        <> O.metavar "NAME"
        <> O.help "Display name of the variable")
      <*> O.option O.auto
        ( O.long "period"
        <> O.short 't'
        <> O.metavar "T"
        <> O.value 1.0
        <> O.help "Refresh period in seconds" )
      <*> O.option O.auto
        ( O.long "field"
        <> O.short 'i'
        <> O.metavar "INDEX"
        <> O.value 1
        <> O.help "Index of the variable in a given message" )

descString :: String
descString = "Plots data from Ivy bus in terminal. For example: "
    ++ "brick-devlunch -r \"(.*) TELEMETRY_STATUS (.*)\""
    ++ " -n ping_time -t 1.0 -i 11"
