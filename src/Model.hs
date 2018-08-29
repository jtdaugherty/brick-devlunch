{-# LANGUAGE TemplateHaskell #-}
module Model
  ( St
  , step
  , initialState
  , initialDelay
  , getString
  , getValues
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

initialState :: TVar Int -> TVar Double -> St
initialState delay src = St 1 [] " N/A" delay src 100

step:: St -> IO St
step st = do
    src <- atomically $ readTVar (st ^. source)
    delay <- atomically $ readTVar (st ^. delayMs)
    return $ st & values %~ ( take (st ^. storageLen) . (src :) )
                & delayVal .~ (show $ (fromIntegral delay) / 1000000)

getString :: St -> String
getString st = ("AcID: " ++ show (st ^. acId)
                ++ " delay: " ++ (st ^. delayVal) ++ " [s]")

getValues :: St -> [Double]
getValues st = st ^. values

