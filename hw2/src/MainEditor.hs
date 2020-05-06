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
  , vBox
  , txtWrap
  )
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Center (centerLayer, hCenter)
import Brick.Widgets.Core
  ( (<+>)
  , hLimit
  , padLeftRight
  , padTopBottom
  , str
  , strWrap
  , withDefAttr
  )
import qualified Brick.Widgets.Edit as Edit
import Control.Monad.Cont (join)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack, unpack, lines)
import Graphics.Vty (Event(..), Key(..))
import IO (Action(..), Entity(..), InfoDir(..), InfoFile(..), MState(..))
import Manager
  ( displayMore
  , displayPath
  , displayPerms
  , displaySize
  , displayTimes
  , handleMakeDirEvent
  , handleSearchEvent
  , handleTouchEvent
  )
import VCS (handleAddEntity)

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
    
helpContent :: [String]
helpContent = [ "Ctrl + Z - return to parent entity"
              , "Ctrl + P - print info about entity"
              , "Ctrl + F - search"
              , "Ctrl + R - remove entity"
              , "Ctrl + D - make a directory"
              , "Ctrl + T - touch a file"
              , "Ctrl + V - vcs init"
              , "Ctrl + A - add file to vcs"
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
    DisplayHelp -> map strWrap helpContent
    DisplayInfo en -> 
      map strWrap . (displaySize en :) $
      displayPerms en ++ displayTimes en ++ [displayPath en, displayMore en]
    Search edit _ -> [str "Search for:", renderEdit0r edit, editorForSearch st]
    Mkdir edit _ -> [str "Input dircetory name:", renderEdit0r edit]
    Touch edit _ -> [str "Input file name (with extension):", renderEdit0r edit]
    VCSAdditionalComment edit _ -> [str "Add comment:", renderEdit0r edit]

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
    Mkdir edit _ -> return $ handleMakeDirEvent st (unpack $ editorLine edit)
    Touch edit _ -> handleTouchEvent st (unpack $ editorLine edit)
    Search _ _ -> return $ exit st
    VCSAdditionalComment _ _txt -> return $ handleAddEntity st _txt
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
        Mkdir _ _ -> (action state) {mEditor = nEditor}
        Touch _ _ -> (action state) {tEditor = nEditor}
        _ -> error "Unexpected pattern"

updateCommentEditor :: TextEditor -> MState -> MState
updateCommentEditor nEdit state = state {action = _action}
  where
    _action = (action state) {aEditor = nEdit, aComment = editorLine nEdit}

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
        DisplayFile _ -> continue st
        DisplayHelp -> continue st
        Mkdir edit _ -> do
          nEditor <- Edit.handleEditorEvent ev edit
          continue $ updateEditor nEditor st
        Touch edit _ -> do
          nEditor <- Edit.handleEditorEvent ev edit
          continue $ updateEditor nEditor st
        Search edit _ -> do
          nEditor <- Edit.handleEditorEvent ev edit
          continue $ updateSearchEditor nEditor st
        VCSAdditionalComment edit _ -> do
          nEditor <- Edit.handleEditorEvent ev edit
          continue $ updateCommentEditor nEditor st
        Nothing_ -> error "Unreachable pattern"