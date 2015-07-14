module Model
  ( Board
  , Player(..)
  , newBoard
  , nextPlayer
  , gameState
  , move
  )
where

import Control.Applicative ((<$>))
import Control.Lens ((^.), (^..), (.~), (&), to, folded)
import Data.Monoid ((<>))
import Data.Maybe (isJust, isNothing)
import Linear (V3(..), _x, _y, _z, transpose)

data Player = X | O
    deriving (Eq, Show)

data Board = Board (V3 (V3 (Maybe Player)))
    deriving (Show)

data GameState =
    InProgress
    | NoMovesLeft
    | Won Player
    deriving (Eq, Show)

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

newBoard :: Board
newBoard = Board (V3 row row row)
    where
        row = V3 Nothing Nothing Nothing

gameState :: Board -> GameState
gameState b =
    if playerWon X b then Won X
    else if playerWon O b then Won O
         else if hasFreeMoves b then InProgress
              else NoMovesLeft

hasFreeMoves :: Board -> Bool
hasFreeMoves (Board m) =
    or $ m^..folded.folded.to isNothing

playerWon :: Player -> Board -> Bool
playerWon p (Board m) =
    or [ playerWonRows p m
       , playerWonRows p $ transpose m
       , playerWonDiagonal p m
       ]

playerWonRows :: Player -> V3 (V3 (Maybe Player)) -> Bool
playerWonRows p m =
    let v = Just p
    in or $ (== (V3 v v v)) <$> [m^._x, m^._y, m^._z]

playerWonDiagonal :: Player -> V3 (V3 (Maybe Player)) -> Bool
playerWonDiagonal p m =
    let v = Just p
    in (m^._x._x, m^._y._y, m^._z._z) == (v, v, v)

move :: (Int, Int) -> (Player, Board) -> (Player, Board)
move _ (p, b) | gameState b /= InProgress = (p, b)
move loc (p, b@(Board m)) | m^.(spaceLens loc).to isJust = (p, b)
move loc (p, Board m) = (nextPlayer p, Board new)
    where
        new = m & (spaceLens loc) .~ (Just p)

spaceLens (r, c) = (getRowL r).(getColL c)
    where
        getRowL v = case v of
                      1 -> _x
                      2 -> _y
                      3 -> _z
                      _ -> error $ "Invalid row position: " <> (show v)
        getColL v = case v of
                      1 -> _x
                      2 -> _y
                      3 -> _z
                      _ -> error $ "Invalid column position: " <> (show v)
