module MainWindow where

import Brick (Widget(..), (<+>), hBox, str, vBox, EventM, BrickEvent(..), Next, continue)
import Brick.Widgets.Border (borderElem, hBorderWithLabel, vBorder)
import Brick.Widgets.Border.Style (bsCornerBL, bsCornerBR)
import Brick.Widgets.Core (fill, hLimit, strWrap, vLimit)
import Brick.Widgets.List
  ( List
  , handleListEvent
  , list
  , listElements
  , listElementsL
  , listItemHeightL
  , listMoveTo
  , listNameL
  , listReplace
  , listSelectedElement
  , listSelected
  , renderList
  , GenericList(..)
  )
import Data.HashMap.Lazy as DHL (lookup)
import Data.Maybe (fromJust)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import IO
  ( Entity(..)
  , InfoDir(..)
  , InfoFile(..)
  , MState(..)
  , MapFPtoEntity
  )
import Manager (Content(..), updateList, renderSize)
import System.Directory (Permissions(..))
import Graphics.Vty (Event(..))
import MainEditor (render, TextEditor)

newtype TextEditorState = State { editor :: TextEditor }

renderCurDirH :: MState -> Widget String
renderCurDirH s = str ((dPath . dir . curEntry) s) <+> fill ' '

renderCurDirH' :: Widget String
renderCurDirH' = str " Current directory "

--renderSearchH :: MState -> Widget String
--renderSearchH s =
--  case search s of
--    Empty -> str "Sorry, nothing was found for your search" <+> fill ' '
--    New -> str "Please input filename" <+> fill ' '
--    SearchInfo _ _ -> str "Search result" <+> fill ' '

renderSearchH' :: Widget String
renderSearchH' = str " Search info "

renderFileH :: MState -> Widget String
renderFileH _ = str "File content"

renderFileH' :: Widget String
renderFileH' = str ""

renderHeaderWidget :: MState -> Content -> (Widget String, Widget String)
--renderHeaderWidget s SearchContent = (renderSearchH s, renderSearchH')
renderHeaderWidget s DirContent = (renderCurDirH s, renderCurDirH')
renderHeaderWidget s FileContent = (renderFileH s, renderFileH')

renderHeaderBox :: MState -> Content -> Widget String
renderHeaderBox s c = vLimit 2 $ vBox [middle, bottom]
  where
    (info, title) = renderHeaderWidget s c
    middle = hBox [vBorder, hLimit 2 $ fill ' ', info <+> fill ' ', vBorder]
    bottom =
      hBox
        [borderElem bsCornerBL, hBorderWithLabel title, borderElem bsCornerBR]

perms :: Entity -> Maybe Permissions
perms (Dir info _ _) = dPerms info
perms (File info _) = fPerms info

renderPerms :: Maybe Permissions -> Widget String
renderPerms Nothing = str " ----"
renderPerms (Just p) =
  str
    [ ' '
    , if readable p
        then 'r'
        else '-'
    , if writable p
        then 'w'
        else '-'
    , if executable p
        then 'x'
        else '-'
    , if searchable p
        then 's'
        else '-'
    ]

times :: Entity -> Maybe (UTCTime, UTCTime)
times (File info _) = fTimes info
times _ = Nothing

accessTime :: Maybe (UTCTime, UTCTime) -> Maybe UTCTime
accessTime x = fst <$> x

modifyTime :: Maybe (UTCTime, UTCTime) -> Maybe UTCTime
modifyTime x = snd <$> x

renderTimes :: Maybe UTCTime -> Widget String
renderTimes modTime =
  str $
  case modTime of
    Just mtm -> formatTime defaultTimeLocale " %R %b %e %Y" mtm
    _ -> " -----------------"

sizes :: Entity -> Int
sizes (File info _) = fSize info
sizes (Dir info _ _) = dSize info

makeWidgetFromEntity :: MapFPtoEntity -> Bool -> FilePath -> Widget String
makeWidgetFromEntity _map _ fp =
  vLimit 1 $
  hBox
    [ str $ show entity
    , fill ' '
    , str $ renderSize $ sizes entity
    , renderPerms $ perms entity
    , renderTimes $ modifyTime $ times entity
    ]
  where
    entity = fromJust $ DHL.lookup fp _map 

renderCurDirC :: MState -> Widget String
renderCurDirC s = renderList (makeWidgetFromEntity $ refMap s) True _list
  where
    _list = (dEntryList . dir . curEntry) s

renderFileC :: MState -> Widget String
--renderFileC s = MainEditor.render ((fContent . file . curEntry) s)
renderFileC s = str "asd"
  
emptyContent :: Widget String
emptyContent = vBox (lns ++ [fill ' '])
  where
    lns =
      map strWrap $
      lines
        "Command Line Interface File Manager\n\
        \A couple words about my manager"

renderContainer :: MState -> Content -> Widget String
renderContainer s DirContent = renderCurDirC s
renderContainer s FileContent = renderFileC s

draw :: MState -> Content -> Widget String
draw state content = vBox [header, container]
  where
    header = renderHeaderBox state content
    container =
      hBox [vBorder, renderContainer state content, vBorder]

------------------------------------------------------------------------
-----------------------------Handle Events------------------------------
------------------------------------------------------------------------

handleEvent :: Event -> MState -> EventM String (Next MState)
handleEvent ev st = do
  op <- handleListEvent ev _list
  continue $ updateList op st
  where
    _list = (dEntryList . dir . curEntry) st