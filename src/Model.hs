{-# LANGUAGE TemplateHaskell #-}
module Model
  ( St
  , step
  , initialState
  , initialDelay
  , getString
  , getValues
  , setTimer
  )
where

import Control.Applicative ((<$>))
import Control.Monad (void, forever)
import Control.Lens
import Data.List (intersperse)
import Data.Monoid
import Control.Concurrent.STM

data St =
    St { _acId :: Int -- ID of the aircraft
       , _values :: [Double] -- list of values to be displayed
       , _delayVal :: String -- string representation of the delay
       , _delayMs :: TVar Int -- controls display speed
       , _source :: TVar Double -- at every step update `values` with `source`
       , _storageLen :: Int -- how many datapoints we keep 
       }

makeLenses ''St 

initialDelay :: Int
initialDelay = 1000000 -- 1 [s]

initialState :: TVar Int -> TVar Double -> IO St
initialState delay src = do
  let st = St 1 [] "N/A" delay src 1000
  setTimer (id) st


step:: St -> IO St
step st = do
    src <- atomically $ readTVar (st ^. source)
    return $ st & values %~ ( take (st ^. storageLen) . (src :) )

setTimer :: (Int -> Int) -> St -> IO St
setTimer f st = do
  delay <- atomically $ do
    modifyTVar (st ^. delayMs) f
    readTVar (st ^. delayMs)
  return $ st & delayVal .~ (show $ (fromIntegral delay) / 1000000)


getString :: St -> String
getString st = ("AcID: " ++ show (st ^. acId)
                ++ " delay: " ++ (st ^. delayVal) ++ " [s]")

getValues :: St -> [Double]
getValues st = st ^. values

