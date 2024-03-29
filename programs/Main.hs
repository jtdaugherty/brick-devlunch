{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative ((<$>))
import Control.Monad (void)
import Control.Lens
import Data.Monoid
import Graphics.Vty

import Model

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border.Style
import Brick.Widgets.Table

data St =
    St { _gameState :: GameState
       , _cursor :: (Int, Int)
       }

makeLenses ''St

initialState :: St
initialState = St (GameState newBoard X) (0, 0)

cursorAttr :: AttrName
cursorAttr = attrName "cursor"

boardAttr :: AttrName
boardAttr = attrName "board"

drawUI :: St -> [Widget ()]
drawUI (St (GameState b p) cur) = [ui]
    where
        ui = center $ withBorderStyle unicode $
             currentPlayer p <=>
             drawBoard cur b <=>
             drawGameStatus b
        currentPlayer pl = str $ "Current player: " <> (show pl)
        drawGameStatus st =
            case gameStatus st of
                Won winner -> str $ show winner <> " won!"
                InProgress -> str "Make a move."
                NoMovesLeft -> str "No moves left, game over!"

drawBoard :: (Int, Int) -> Board -> Widget ()
drawBoard cur board =
    withDefAttr boardAttr $ renderTable (table rows)
    where
        rows = drawRow <$> zip [0..] (toList board)
        drawRow (r, row) =
            [ drawPiece ((r, c) == cur) piece
            | (c, piece) <- zip [0..] row
            ]

drawPiece :: Bool -> Maybe Player -> Widget ()
drawPiece selected piece =
    let w = str $ maybe " " show piece
        attr = if selected then cursorAttr else boardAttr
    in withAttr attr $
       padLeftRight 2 $
       padTopBottom 1 w

moveWith :: Lens' (Int, Int) Int -> (Int -> Int) -> EventM () St ()
moveWith which f = cursor.which %= (min 2 . max 0 . f)

moveVert, moveHoriz :: (Int -> Int) -> EventM () St ()
moveVert  = moveWith _1
moveHoriz = moveWith _2

moveUp, moveDown, moveLeft, moveRight :: EventM () St ()
moveUp    = moveVert (subtract 1)
moveDown  = moveVert (+ 1)
moveLeft  = moveHoriz (subtract 1)
moveRight = moveHoriz (+ 1)

appEvent :: BrickEvent () e -> EventM () St ()
appEvent (VtyEvent (EvKey k [])) =
    case k of
        -- Reset
        KChar 'r' -> put initialState

        -- Cursor movement
        KUp    -> moveUp
        KLeft  -> moveLeft
        KDown  -> moveDown
        KRight -> moveRight

        -- Make a move at the cursor position
        KChar ' ' -> do
            cur <- use cursor
            gameState %= move cur

        -- Quit
        KChar 'q' -> halt
        KEsc      -> halt

        _          -> return ()
appEvent _ = return ()

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
        , appStartEvent = return ()
        }

main :: IO ()
main = void $ defaultMain app initialState
