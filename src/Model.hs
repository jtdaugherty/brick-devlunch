{-# LANGUAGE TemplateHaskell #-}
module Model
  ( St
  , step
  , initialState
  , getString
  , getLocalNoise
  , getNoiseValues
  )
where

import qualified Ivy as I

import Control.Applicative ((<$>))
import Control.Monad (void, forever)
import Control.Lens
import Data.List (intersperse)
import Data.Monoid

data St =
    St { _acId :: Int -- ID of the aircraft
       , _values :: [Int] -- list of values to be displayed
       , _delayVal :: String -- string representation of the delay
       , _delayMs :: TVar Int -- controls display speed
       , _source :: TVar Int -- at every step update `values` with `source`
       }
    deriving Show

makeLenses ''St 

startIvyLoop :: TVar Int -> IO ()
startIvyLoop source = do
    app_name <- newCString "hello app"
    ready_msg <- newCString "hello app ready"
    I.ivyInit app_name ready_msg nullPtr nullPtr nullPtr nullPtr
    addr <- newCString ""
    I.ivyStart addr
    regexp <- newCString "ground TELEMETRY_STATUS (.*)"
    -- 1 -1 0.021499 14234 509 686.0 4 4 4 0 702 14.36
    cb <- I.createIvyCb $ myCallback source -- closing over myVar
    I.ivyBindMsg cb nullPtr regexp
    I.ivyMainLoop -- start main loop

myCallback:: TVar [Int] -> Ptr a -> Ptr a -> Int -> Ptr (CString) -> IO ()
myCallback src _ _ _ valPtr = do
    val <- peek valPtr
    str <- peekCString val
    putStrLn $ show (splitOn str)
    -- update TVar
    atomically $ writeTVar src (last str)

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                          "" -> []
                          s' -> w : wordsWhen p s''
                                where (w, s'') = break p s'

splitOn :: String -> [String]
splitOn = wordsWhen (==' ') 

  

initialDelay :: Int
initialDelay = 1000000 -- 1 [s]

initialState :: TVar Int -> TVar Int -> St
initialState delay src = St 1 [] delay src

step:: St -> IO St
step st = do
    src <- atomically $ readTVar st & source
    delay <- atomically $ readTVar st & delayMs
    st & values .~ (show delay)
    return $ st & values %~ ( take 20 . (src :) ) -- easier replacement

getString :: St -> String
getString st = ("AcID: " ++ show (st ^. acId) ++ show (st ^. delayVal))

getValues :: St -> [Int]
getValues st = st ^. values

