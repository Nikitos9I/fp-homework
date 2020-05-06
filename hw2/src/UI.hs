module UI where

import Brick (AttrName, BrickEvent(..), EventM, Next, Widget(..), App(..), showFirstCursor, attrMap, defaultMain, attrName, vBox, AttrName(..), strWrap, str)
import Brick.Main (halt, continue)
import Graphics.Vty
import IO (Entity(..), MState(..), Action(..), InfoDir(..), InfoFile(..), TextEditor)
import MainWindow
import Footer
import Control.Monad ((<=<))
import Brick.Themes (Theme, themeToAttrMap, newTheme)
import Brick.Util (on, fg, bg)
import Brick.Widgets.Edit (editFocusedAttr, renderEditor)
import Brick.Widgets.List (listSelectedFocusedAttr, listSelectedAttr)
import Manager (openEntry, getMode, goBack, openSearch, deleteEntity, displayInfo, makeDirectory, makeFile)
import MainEditor (handleEvent, render, editorAttr, keybindAttr)
import Debug.Trace

drawResults :: MState -> [Widget String]
drawResults s = [ui]
  where
    ui = vBox widgets
    widgets = [MainWindow.draw s $ getMode s, Footer.draw]
       
draw :: MState -> [Widget String]
--draw s | trace (show $ action s) False = undefined
draw s = case action s of 
  Nothing_ -> drawResults s
  _ -> [MainEditor.render s]

handleMain :: MState -> BrickEvent String e -> EventM String (Next MState)
--handleMain s (VtyEvent ev) | trace (show ev) False = undefined
handleMain s (VtyEvent ev) = case ev of
  EvKey KEsc [] -> halt s
  EvKey KEnter [] -> openEntry s
  EvKey (KChar 'f') [MCtrl] -> openSearch s
  EvKey (KChar 'z') [MCtrl] -> goBack s
  EvKey (KChar 'r') [MCtrl] -> deleteEntity s
  EvKey (KChar 'p') [MCtrl] -> displayInfo s
  EvKey (KChar 'd') [MCtrl] -> makeDirectory s
  EvKey (KChar 't') [MCtrl] -> makeFile s
  _ -> MainWindow.handleEvent ev s
handleMain s _ = continue s

handleEvent :: MState -> BrickEvent String e -> EventM String (Next MState)
handleEvent state ev = case action state of
  Nothing_ -> handleMain state ev
  _ -> MainEditor.handleEvent ev state

errorAttr :: AttrName
errorAttr = attrName "error"

defaultTheme :: Theme
defaultTheme = newTheme (brightCyan `on` brightBlack) [
    (listSelectedAttr, fg yellow),
    (listSelectedFocusedAttr, black `on` yellow),
    (keybindAttr, fg white `withStyle` underline),
    (editorAttr, bg brightBlack),
    (errorAttr, bg red),
    (editFocusedAttr, black `on` yellow)
--    (disclaimerAttr, black `on` white)
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

