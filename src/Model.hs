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

import Control.Lens
import Control.Concurrent.STM

data St =
    St { _acId :: Int -- ID of the aircraft
       , _values :: [Double] -- list of values to be displayed
       , _delayVal :: String -- string representation of the delay
       , _delayMs :: TVar Int -- controls display speed
       , _source :: TVar Double -- at every step update `values` with `source`
       , _storageLen :: Int -- how many datapoints we keep 
       , _regexpr :: String -- regular expression used
       }

makeLenses ''St 

initialDelay :: Int
initialDelay = 1000000 -- 1 [s]

initialState :: TVar Int -> TVar Double -> String -> IO St
initialState delay src expr = do
  let st = St 1 [] "N/A" delay src 1000 expr
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
  return $ st & delayVal .~ (show $ (fromIntegral delay :: Double) / 1000000)


getString :: St -> String
getString st = ("AcID: " ++ show (st ^. acId)
                ++ " period: " ++ (st ^. delayVal) ++ " [s], " ++ (st ^. regexpr))

getValues :: St -> [Double]
getValues st = st ^. values

