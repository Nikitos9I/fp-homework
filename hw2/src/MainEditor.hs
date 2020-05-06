module MainEditor where

import Brick (EventM, Next, Widget, continue, txt, vBox, AttrName, attrName, BrickEvent(..))
import qualified Brick.Widgets.Edit as Edit
import Data.Text (Text, pack, unpack)
import Graphics.Vty (Event(..))
import IO (Entity(..), InfoDir(..), InfoFile(..), MState(..), Action(..))
import Brick.Widgets.Center (centerLayer, hCenter)
import Brick.Widgets.Border (hBorder, borderWithLabel)
import Brick.Widgets.Core (withDefAttr, str, hLimit, padLeftRight, padTopBottom, strWrap, (<+>))
import Manager (displaySize, displayPerms, displayTimes, displayPath, displayMore, handleMakeDirEvent, handleTouchEvent, handleSearchEvent)
import Graphics.Vty
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Cont (join)

type TextEditor = Edit.Editor Text String

editorAttr :: AttrName
editorAttr = attrName "editor"

keybindAttr :: AttrName
keybindAttr = attrName "keybinding"

editorLine :: TextEditor -> Text
editorLine = head . Edit.getEditContents

updateContent :: TextEditor -> MState -> MState
updateContent edt x = x {curEntry = entry}
  where
    entry = (curEntry x) {file = infoFile}
    infoFile = ((file . curEntry) x) {fContent = pack "edt"}

------------------------------------------------------------------------
-------------------------------- Render --------------------------------
------------------------------------------------------------------------

editorForSearch :: MState -> Widget String
editorForSearch st = vBox $ str "Search results:" : map (str . show) (handleSearchEvent st)  

renderContent :: [Text] -> Widget String
renderContent lst = vBox $ map txt lst

renderEdit0r :: TextEditor -> Widget String
renderEdit0r = Edit.renderEditor renderContent False

renderBody :: MState -> Widget String
renderBody st = vBox $ case action st of
  DisplayInfo en -> map strWrap . (displaySize en :) $ displayPerms en ++ displayTimes en ++ [displayPath en, displayMore en]
  Search edit _ -> [str "Search for:", renderEdit0r edit, editorForSearch st]
  Mkdir edit _ -> [str "Input dircetory name:", renderEdit0r edit]
  Touch edit _ -> [str "Input file name (with extension):", renderEdit0r edit]

renderFooter :: Action -> Widget String
renderFooter act =
  kb "Enter" <+> str _txt <+> kb "Esc" <+> str " to close and go back"
  where
    kb = withDefAttr keybindAttr . str
    _txt =
      case act of
        Mkdir _ _ -> " to make the directory, "
        Touch _ _ -> " to touch the file, "
        Search _ _ -> " to end search, "
        _ -> " or "

render :: MState -> Widget String
render state = centerLayer . box $ vBox [body, hBorder, footer]
  where
    box = withDefAttr editorAttr . borderWithLabel (str . show $ action state)
    body = padLeftRight 2 . padTopBottom 1 $ renderBody state
    footer = hCenter . renderFooter $ action state

------------------------------------------------------------------------
---------------------------- Handle Events -----------------------------
------------------------------------------------------------------------

exit :: MState -> MState
exit s = s {action = Nothing_}

performAction :: MState -> IO MState
performAction st = case action st of
  DisplayInfo _ -> return $ exit st
  DisplayError _ -> return $ exit st
  Mkdir edit _ -> return $ handleMakeDirEvent st (unpack $ editorLine edit)
  Touch edit _ -> handleTouchEvent st (unpack $ editorLine edit)
  Search _ _ -> return $ exit st
  Nothing_ -> error "Unreachable pattern"

updateSearchEditor :: TextEditor -> MState -> MState
updateSearchEditor nEdit state = state {action = _action} 
  where
    _action = (action state) {sEditor = nEdit, sQuery = editorLine nEdit}

updateEditor :: TextEditor -> MState -> MState
updateEditor nEditor state = state { action = _action}
  where
    _action = case action state of
      Mkdir _ _ -> (action state) {mEditor = nEditor}
      Touch _ _ -> (action state) {tEditor = nEditor}
      _ -> error "Unexpected pattern"

handleEvent :: BrickEvent String e -> MState -> EventM String (Next MState)
handleEvent (AppEvent ev) st = undefined
handleEvent (VtyEvent ev) st =
  case ev of
    EvKey KEsc [] -> continue $ exit st
    EvKey KEnter [] -> join (liftIO $ continue <$> performAction st)
    _ ->
      case action st of
        DisplayInfo _ -> continue st
        DisplayError _ -> continue st
        Mkdir edit _ -> do
          nEditor <- Edit.handleEditorEvent ev edit
          continue $ updateEditor nEditor st
        Touch edit _ -> do
          nEditor <- Edit.handleEditorEvent ev edit
          continue $ updateEditor nEditor st
        Search edit _ -> do
          nEditor <- Edit.handleEditorEvent ev edit
          continue $ updateSearchEditor nEditor st
        Nothing_ -> error "Unreachable pattern"
