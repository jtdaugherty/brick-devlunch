module Model
  ( Board
  , Player(..)
  , GameStatus(..)
  , newBoard
  , toList
  , nextPlayer
  , gameStatus
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

data GameStatus =
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

toList :: Board -> [[Maybe Player]]
toList (Board m) = [m^.._x.folded, m^.._y.folded, m^.._z.folded]

gameStatus :: Board -> GameStatus
gameStatus b =
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
    in (v, v, v) `elem` [ (m^._x._x, m^._y._y, m^._z._z)
                        , (m^._x._z, m^._y._y, m^._z._x)
                        ]

move :: (Int, Int) -> (Player, Board) -> (Player, Board)
move _ (p, b) | gameStatus b /= InProgress = (p, b)
move loc (p, b@(Board m)) | m^.(spaceLens loc).to isJust = (p, b)
move loc (p, Board m) = (nextPlayer p, Board new)
    where
        new = m & (spaceLens loc) .~ (Just p)

spaceLens (r, c) = (getRowL r).(getColL c)
    where
        getRowL v = case v of
                      0 -> _x
                      1 -> _y
                      2 -> _z
                      _ -> error $ "Invalid row position: " <> (show v)
        getColL v = case v of
                      0 -> _x
                      1 -> _y
                      2 -> _z
                      _ -> error $ "Invalid column position: " <> (show v)
