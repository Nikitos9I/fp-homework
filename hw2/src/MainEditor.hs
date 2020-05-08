module MainEditor where

import Brick
  ( AttrName
  , BrickEvent(..)
  , EventM
  , Next
  , Widget
  , attrName
  , continue
  , txt
  , txtWrap
  , vBox
  )
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Center (centerLayer, hCenter)
import Brick.Widgets.Core
  ( (<+>)
  , padLeftRight
  , padTopBottom
  , str
  , strWrap
  , withDefAttr
  )
import qualified Brick.Widgets.Edit as Edit
import Control.Monad.Cont (join)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, lines, unpack)
import Graphics.Vty (Event(..), Key(..))
import IO (Action(..), MState(..))
import Manager
  ( displayMore
  , displayPath
  , displayPerms
  , displaySize
  , displayTimes
  , handleMakeDirEvent
  , handleSearchEvent
  , handleTouchEvent
  , handleWriteToFile
  )
import VCS (handleAddChangedEntity)

type TextEditor = Edit.Editor Text String

editorAttr :: AttrName
editorAttr = attrName "editor"

keybindAttr :: AttrName
keybindAttr = attrName "keybinding"

errorAttr :: AttrName
errorAttr = attrName "error"

editorLine :: TextEditor -> Text
editorLine = head . Edit.getEditContents

helpContent :: [String]
helpContent =
  [ "Ctrl + Z - return to parent entity"
  , "Ctrl + P - print info about entity"
  , "Ctrl + F - search"
  , "Ctrl + R - remove entity"
  , "Ctrl + D - make a directory"
  , "Ctrl + T - touch a file"
  , "Ctrl + V - vcs init"
  , "Ctrl + A - add file to vcs"
  , "Ctrl + L - show list of file revision in vcs (and after `Enter` for removing revision)"
  , "Shift + R - show files under vcs (and after `Enter` for removing file from vcs)"
  , "Enter - go to selected entity/open file for reading"
  , "Esc - exit from file manager"
  ]

------------------------------------------------------------------------
-------------------------------- Render --------------------------------
------------------------------------------------------------------------
editorForSearch :: MState -> Widget String
editorForSearch st =
  vBox $ str "Search results:" : map (str . show) (handleSearchEvent st)

renderContent :: [Text] -> Widget String
renderContent lst = vBox $ map txt lst

renderEdit0r :: TextEditor -> Widget String
renderEdit0r = Edit.renderEditor renderContent False

renderBody :: MState -> Widget String
renderBody st =
  vBox $
  case action st of
    DisplayFile _txt -> map txtWrap (Data.Text.lines _txt)
    DisplayError _txt -> [(withDefAttr errorAttr . str) _txt]
    DisplayHelp -> map strWrap helpContent
    DisplayInfo en ->
      map strWrap . (displaySize en :) $
      displayPerms en ++ displayTimes en ++ [displayPath en, displayMore en]
    Search edit _ -> [str "Search for:", renderEdit0r edit, editorForSearch st]
    Mkdir edit -> [str "Input directory name:", renderEdit0r edit]
    Touch edit -> [str "Input file name (with extension):", renderEdit0r edit]
    Write edit _ -> [str "Input text:", renderEdit0r edit]
    VCSAdditionalComment edit _ -> [str "Add comment:", renderEdit0r edit]
    Nothing_ -> error "Unexpected pattern"

renderFooter :: Action -> Widget String
renderFooter act =
  kb "Enter" <+> str _txt <+> kb "Esc" <+> str " to close and go back"
  where
    kb = withDefAttr keybindAttr . str
    _txt =
      case act of
        Mkdir _ -> " to make the directory, "
        Touch _ -> " to touch the file, "
        Search _ _ -> " to end search, "
        Write _ _ -> " to write to file "
        VCSAdditionalComment _ _ -> " to add comment, "
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
performAction st =
  case action st of
    DisplayInfo _ -> return $ exit st
    DisplayFile _ -> return $ exit st
    DisplayError _ -> return $ exit st
    DisplayHelp -> return $ exit st
    Mkdir edit -> return $ handleMakeDirEvent st (unpack $ editorLine edit)
    Touch edit -> handleTouchEvent st (unpack $ editorLine edit)
    Search _ _ -> return $ exit st
    Write edit fp -> handleWriteToFile st (editorLine edit) fp
    VCSAdditionalComment _ _txt -> return $ handleAddChangedEntity st _txt
    Nothing_ -> error "Unreachable pattern"

updateSearchEditor :: TextEditor -> MState -> MState
updateSearchEditor nEdit state = state {action = _action}
  where
    _action = (action state) {sEditor = nEdit, sQuery = editorLine nEdit}

updateEditor :: TextEditor -> MState -> MState
updateEditor nEditor state = state {action = _action}
  where
    _action =
      case action state of
        Mkdir _ -> (action state) {mEditor = nEditor}
        Touch _ -> (action state) {tEditor = nEditor}
        Write _ _ -> (action state) {wEditor = nEditor}
        _ -> error "Unexpected pattern"

updateCommentEditor :: TextEditor -> MState -> MState
updateCommentEditor nEdit state = state {action = _action}
  where
    _action = (action state) {aEditor = nEdit, aComment = editorLine nEdit}

handleEvent :: BrickEvent String e -> MState -> EventM String (Next MState)
handleEvent (AppEvent _) _ = error "Unexpected event"
handleEvent MouseUp {} _ = error "Unexpected event"
handleEvent MouseDown {} _ = error "Unexpected event"
handleEvent (VtyEvent ev) st =
  case ev of
    EvKey KEsc [] -> continue $ exit st
    EvKey KEnter [] -> join (liftIO $ continue <$> performAction st)
    _ ->
      case action st of
        DisplayInfo _ -> continue st
        DisplayError _ -> continue st
        DisplayFile _ -> continue st
        DisplayHelp -> continue st
        Mkdir edit -> do
          nEditor <- Edit.handleEditorEvent ev edit
          continue $ updateEditor nEditor st
        Touch edit -> do
          nEditor <- Edit.handleEditorEvent ev edit
          continue $ updateEditor nEditor st
        Write edit _ -> do
          nEditor <- Edit.handleEditorEvent ev edit
          continue $ updateEditor nEditor st
        Search edit _ -> do
          nEditor <- Edit.handleEditorEvent ev edit
          continue $ updateSearchEditor nEditor st
        VCSAdditionalComment edit _ -> do
          nEditor <- Edit.handleEditorEvent ev edit
          continue $ updateCommentEditor nEditor st
        Nothing_ -> error "Unreachable pattern"