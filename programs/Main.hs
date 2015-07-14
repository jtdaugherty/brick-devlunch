{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative ((<$>))
import Control.Monad (void)
import Control.Lens
import Data.Default
import Data.Monoid
import Graphics.Vty

import Model

import Brick.Main
import Brick.Util
import Brick.AttrMap
import Brick.Widgets.Core
import Brick.Widgets.Border

main :: IO ()
main = return ()
