{-# LANGUAGE TemplateHaskell #-}
module Model
  ( St
  , step
  , initializeState
  , getString
  , getValues
  , getPeriod
  , setTimer
  )
where

import qualified Ivy as I
import Control.Lens
import Control.Concurrent.STM
import Control.Monad (void)
import Control.Concurrent (forkIO)

data St =
    St { _acId :: TVar String -- ID of the data source
       , _acIdVal :: String -- string representation of the data source
       , _values :: [Double] -- list of values to be displayed
       , _delayVal :: String -- string representation of the delay
       , _delayMs :: TVar Int -- controls display speed
       , _source :: TVar Double -- at every step update `values` with `source`
       , _storageLen :: Int -- how many datapoints we keep 
       , _name :: String -- name of the displayed data field
       }

makeLenses ''St 

-- `delay_var` is shared with the timer thread
-- `expr` is the rexepr for the Ivy bus
-- `name` is the name of the data field
-- `idx` is the index of the variable within the binded message
initializeState :: TVar Int -> String -> String -> Int -> IO St
initializeState delayVar expr name idx = do
  dataVar <- atomically $ newTVar 1.0
  sourceVar <- atomically $ newTVar "N/A"
  void $ forkIO $ I.ivyMain dataVar sourceVar expr idx
  let st = St sourceVar "N/A" [] "N/A" delayVar dataVar 1000 name
  setTimer (id) st

-- get new datapoint and store it
-- get new source name and store it
step:: St -> IO St
step st = do
    newDatapoint <- atomically $ readTVar (st ^. source)
    newSource <- atomically $ readTVar (st ^. acId)
    return $ st & values %~ ( take (st ^. storageLen) . (newDatapoint :) )
                & acIdVal .~ newSource

-- update the refresh period
-- clears the current data
setTimer :: (Int -> Int) -> St -> IO St
setTimer f st = do
  delay <- atomically $ do
    modifyTVar (st ^. delayMs) f
    readTVar (st ^. delayMs)
  return $ st & delayVal .~ (show $ (fromIntegral delay :: Double) / 1000000)
              & values .~ []


getString :: St -> String
getString st = ("source ID: " ++ show (st ^. acIdVal)
                ++ " period: " ++ (st ^. delayVal)
                ++ " [s], " ++ (st ^. name))

getValues :: St -> [Double]
getValues st = st ^. values

getPeriod :: St -> Double
getPeriod st = (read (st ^. delayVal) :: Double)
