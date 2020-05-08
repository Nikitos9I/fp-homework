module UI where

import Brick
  ( App(..)
  , AttrName
  , AttrName(..)
  , BrickEvent(..)
  , EventM
  , Next
  , Widget(..)
  , attrMap
  , attrName
  , defaultMain
  , showFirstCursor
  , str
  , strWrap
  , vBox
  , vLimit
  , vLimitPercent
  )
import Brick.Main (continue, halt)
import Brick.Themes (Theme, newTheme, themeToAttrMap)
import Brick.Util (bg, fg, on)
import Brick.Widgets.Edit (editFocusedAttr, renderEditor)
import Brick.Widgets.List (listSelectedAttr, listSelectedFocusedAttr)
import Control.Monad.IO.Class (liftIO)
import Debug.Trace
import Footer
import Graphics.Vty
import IO
  ( Action(..)
  , Entity(..)
  , InfoDir(..)
  , InfoFile(..)
  , MState(..)
  , TextEditor
  , backToFS
  )
import MainEditor (editorAttr, handleEvent, keybindAttr, render, errorAttr)
import MainWindow (draw, handleEvent)
import Manager
  ( actionWithEntry
  , addChangedEntity
  , deleteEntity
  , displayInfo
  , goBack
  , init
  , makeDirectory
  , makeFile
  , openHelp
  , openSearch
  , openVCSList
  , openVCSRevList
  , writeToFile
  )
import Control.Monad.Cont (join)

drawResults :: MState -> [Widget String]
drawResults s = [widgets]
  where
    widgets = vBox [MainWindow.draw s, Footer.draw]

draw :: MState -> [Widget String]
--draw s | trace (show $ action s) False = undefined
draw s =
  case action s of
    Nothing_ -> drawResults s
    _ -> [MainEditor.render s]

handleMain :: MState -> BrickEvent String e -> EventM String (Next MState)
--handleMain s (VtyEvent ev) | trace (show ev) False = undefined
handleMain s (VtyEvent ev) =
  case ev of
    EvKey KEsc [] -> join (liftIO $ halt <$> backToFS s)
    EvKey KEnter [] -> actionWithEntry s
    EvKey (KChar 'f') [MCtrl] -> openSearch s
    EvKey (KChar 'z') [MCtrl] -> goBack s
    EvKey (KChar 'r') [MCtrl] -> deleteEntity s
    EvKey (KChar 'p') [MCtrl] -> displayInfo s
    EvKey (KChar 'd') [MCtrl] -> makeDirectory s
    EvKey (KChar 't') [MCtrl] -> makeFile s
    EvKey (KChar 'v') [MCtrl] -> Manager.init s
    EvKey (KChar 'a') [MCtrl] -> addChangedEntity s
    EvKey (KChar 'o') [MCtrl] -> openHelp s
    EvKey (KChar 'w') [MCtrl] -> writeToFile s
    EvKey (KChar 'l') [MCtrl] -> openVCSRevList s
    EvKey (KChar 'R') [] -> openVCSList s
    _ -> MainWindow.handleEvent ev s
handleMain s _ = continue s

handleEvent :: MState -> BrickEvent String e -> EventM String (Next MState)
handleEvent state ev =
  case action state of
    Nothing_ -> handleMain state ev
    _ -> MainEditor.handleEvent ev state

defaultTheme :: Theme
defaultTheme =
  newTheme
    (brightCyan `on` brightBlack)
    [ (listSelectedAttr, fg yellow)
    , (listSelectedFocusedAttr, black `on` yellow)
    , (keybindAttr, fg white `withStyle` underline)
    , (editorAttr, bg brightBlack)
    , (errorAttr, bg red)
    , (editFocusedAttr, black `on` yellow)
    ]

app :: App MState e String
app =
  App
    { appDraw = UI.draw
    , appChooseCursor = showFirstCursor
    , appHandleEvent = UI.handleEvent
    , appStartEvent = return
    , appAttrMap = const $ themeToAttrMap defaultTheme
    }

run :: MState -> IO ()
run state = do
  _ <- defaultMain app state
  return ()
