module MainEditor where

import Brick (EventM, Next, Widget, continue, txt, vBox, AttrName, attrName, BrickEvent(..))
import qualified Brick.Widgets.Edit as Edit
import Data.Text (Text, pack)
import Graphics.Vty (Event(..))
import IO (Entity(..), InfoDir(..), InfoFile(..), MState(..), Action(..))
import Brick.Widgets.Center (centerLayer, hCenter)
import Brick.Widgets.Border (hBorder, borderWithLabel)
import Brick.Widgets.Core (withDefAttr, str, hLimit, padLeftRight, padTopBottom, strWrap, (<+>))
import Manager (displaySize, displayPerms, displayTimes, displayPath, displayMore)
import Graphics.Vty

type TextEditor = Edit.Editor Text String

editorAttr :: AttrName
editorAttr = attrName "editor"

keybindAttr :: AttrName
keybindAttr = attrName "keybinding"

emptyEditor :: TextEditor
emptyEditor = makeTextEditor (pack "")

makeTextEditor :: Text -> TextEditor
makeTextEditor = Edit.editorText "randomText" (Just 10)

updateContent :: TextEditor -> MState -> MState
updateContent edt x = x {curEntry = entry}
  where
    entry = (curEntry x) {file = infoFile}
    infoFile = ((file . curEntry) x) {fContent = pack "edt"}

------------------------------------------------------------------------
-------------------------------- Render --------------------------------
------------------------------------------------------------------------

renderContent :: [Text] -> Widget String
renderContent lst = vBox $ map txt lst

renderEdit0r :: TextEditor -> Widget String
renderEdit0r = Edit.renderEditor renderContent False

renderBody :: MState -> Widget String
renderBody st = vBox $ case action st of
  DisplayInfo en -> map strWrap . (displaySize en :) $ displayPerms en ++ displayTimes en ++ [displayPath en, displayMore en]
  Search edit _ -> str "Search for:" : [renderEdit0r edit]

renderFooter :: Action -> Widget String
renderFooter act =
  kb "Enter" <+> str _txt <+> kb "Esc" <+> str " to close and go back"
  where
    kb = withDefAttr keybindAttr . str
    _txt =
      case act of
        Mkdir _ _ -> " to make the directory, "
        Touch _ _ -> " to touch the file, "
        GoTo _ -> " to change directory, "
        Search _ _ -> " to search, "
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
performAction = undefined

updateSearchEditor :: TextEditor -> MState -> MState
updateSearchEditor nEditor state = state { action = _action}
  where
    _action = (action state) {editor = nEditor}

handleEvent :: BrickEvent String e -> MState -> EventM String (Next MState)
handleEvent (AppEvent ev) st = undefined
handleEvent (VtyEvent ev) st = case ev of
  EvKey KEsc [] -> continue $ exit st 
  EvKey KEnter [] -> undefined
  _ -> case action st of
    DisplayInfo _ -> continue st
    Search edit _ -> do
      nEditor <- Edit.handleEditorEvent ev edit
      continue $ updateSearchEditor nEditor st
    _ -> undefined
