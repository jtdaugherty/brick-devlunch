{-# LANGUAGE TemplateHaskell #-}
module Model
  ( St
  , step
  , initialState
  , getString
  , getLocalNoise
  , getNoiseValues
  -- , localNoise
  )
where

import qualified System.Random as R

import Control.Applicative ((<$>))
import Control.Monad (void, forever)
import Control.Lens
import Data.List (intersperse)
import Data.Monoid

data St =
    St { _acId :: Int
       , _data :: [Int]
       , _delayMs :: TVar Int
       }
    deriving Show

makeLenses ''St


initialState :: St
initialState = St 1 0 [0] [0] [0] [0]

-- TODO: keep length according to the terminal witdh?
-- TODO: how do I add IO into the equation?
-- TODO: mutable vars?
step:: St -> IO St
--step st = st & localNoise .~ ( take 20 $ ( ((head (st ^. localNoise) ) + 1) `mod` 20 ) : (st ^. localNoise) )
step st = do
    r <- R.getStdRandom $ R.randomR (0, 20)
    return $ st & localNoise %~ ( take 20 . (r :) ) -- easier replacement

getString :: St -> String
getString st = ("AcID: " ++ show (st ^. acId) ++ ", Tx Power: " ++ show (st ^. txPower) ++ " [dB]" )


getLocalNoise :: St -> String
getLocalNoise st = "Local Noise: " ++ show (st ^. localNoise)

getNoiseValues :: St -> [Int]
getNoiseValues st = st ^. localNoise

