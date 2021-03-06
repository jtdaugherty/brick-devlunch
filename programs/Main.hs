{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative ((<$>))
import Control.Monad (void)
import Control.Lens
import Data.List (intersperse)
import Data.Monoid
import Graphics.Vty

import Model

import Brick
import Brick.Main
import Brick.Util
import Brick.AttrMap
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

data St =
    St { _gameState :: (Player, Board)
       , _cursor :: (Int, Int)
       }

makeLenses ''St

initialState :: St
initialState = St (X, newBoard) (0, 0)

cursorAttr :: AttrName
cursorAttr = "cursor"

boardAttr :: AttrName
boardAttr = "board"

drawUI :: St -> [Widget ()]
drawUI (St (p, b) cur) = [ui]
    where
        ui = center $ withBorderStyle unicode $
             currentPlayer p <=>
             drawBoard cur b <=>
             drawGameStatus b
        currentPlayer pl = str $ "Current player: " <> (show pl)
        drawGameStatus b =
            case gameStatus b of
                Won p -> str $ show p <> " won!"
                InProgress -> str "Make a move."
                NoMovesLeft -> str "No moves left, game over!"

drawBoard :: (Int, Int) -> Board -> Widget ()
drawBoard cursor board = border $ withDefAttr boardAttr rows
    where
        rows = vBox $ intersperse rowBorder $
                      drawRow <$> zip [0..] (toList board)
        rowBorder = hBox $ intersperse (borderElem bsIntersectFull) $
                           replicate 3 $ hLimit 5 hBorder
        drawRow (r, row) = hBox $
          intersperse (vLimit 3 vBorder) $
            [ drawPiece ((r, c) == cursor) piece
            | (c, piece) <- zip [0..] row
            ]

drawPiece :: Bool -> Maybe Player -> Widget ()
drawPiece selected piece =
    let w = str $ maybe " " show piece
        attr = if selected then cursorAttr else boardAttr
    in withAttr attr $
       padLeftRight 2 $
       padTopBottom 1 w

appEvent :: St -> BrickEvent () e -> EventM () (Next St)
appEvent st (VtyEvent (EvKey k [])) =
    case k of
        KChar 'r'  -> continue initialState
        KChar 'q'  -> halt st
        KEsc       -> halt st
        KChar ' '  -> continue $ st & gameState %~ move (st^.cursor)
        KUp        -> continue $ st & cursor._1 %~ (max 0 . subtract 1)
        KLeft      -> continue $ st & cursor._2 %~ (max 0 . subtract 1)
        KDown      -> continue $ st & cursor._1 %~ (min 2 . (+ 1))
        KRight     -> continue $ st & cursor._2 %~ (min 2 . (+ 1))
        _          -> continue st
appEvent st _ = continue st

theMap :: AttrMap
theMap = attrMap defAttr
    [ (cursorAttr,  black `on` yellow)
    , (boardAttr,   white `on` blue)
    ]

app :: App St e ()
app =
    App { appDraw = drawUI
        , appHandleEvent = appEvent
        , appAttrMap = const theMap
        , appChooseCursor = neverShowCursor
        , appStartEvent = return
        }

main :: IO ()
main = void $ defaultMain app initialState
