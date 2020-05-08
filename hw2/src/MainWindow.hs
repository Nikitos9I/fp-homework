module MainWindow where

import Brick
  ( BrickEvent(..)
  , EventM
  , Next
  , Widget(..)
  , (<+>)
  , continue
  , hBox
  , str
  , txt
  , vBox
  , vLimitPercent
  )
import Brick.Widgets.Border (borderElem, hBorderWithLabel, vBorder)
import Brick.Widgets.Border.Style (bsCornerBL, bsCornerBR)
import Brick.Widgets.Core (fill, hLimit, strWrap, vLimit)
import Brick.Widgets.List
  ( GenericList(..)
  , List
  , handleListEvent
  , list
  , listElements
  , listElementsL
  , listItemHeightL
  , listMoveTo
  , listNameL
  , listReplace
  , listSelected
  , listSelectedElement
  , renderList
  )
import Data.HashMap.Lazy as DHL (lookup, insert)
import Data.Maybe (fromJust)
import qualified Data.Text as T (lines)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Graphics.Vty (Event(..))
import IO
  ( ContentMode(..)
  , Entity(..)
  , InfoDir(..)
  , InfoFile(..)
  , MState(..)
  , MapFPtoEntity
  , VCS(..)
  , Action(DisplayError)
  )
import Manager (renderSize, updateFileList, updateList, updateVcsList, selectedEntity)
import System.Directory (Permissions(..))
import Errors (fileNotInVcs)

renderCurDirH :: MState -> Widget String
renderCurDirH s = str ((dPath . dir . curEntry) s) <+> fill ' '

renderCurDirH' :: Widget String
renderCurDirH' = str " Current directory "

renderFileH :: Widget String
renderFileH = str "File content"

renderFileH' :: Widget String
renderFileH' = str ""

renderVcsFilesH :: Widget String
renderVcsFilesH = str "Files under VCS"

renderVcsFilesH' :: Widget String
renderVcsFilesH' = str ""

renderVcsRevH :: Widget String
renderVcsRevH = str "History of file changing"

renderVcsRevH' :: Widget String
renderVcsRevH' = str ""

renderHeaderWidget :: MState -> (Widget String, Widget String)
renderHeaderWidget s =
  case mode s of
    DirContent -> (renderCurDirH s, renderCurDirH')
    FileContent -> (renderFileH, renderFileH')
    VCSContent -> (renderVcsFilesH, renderVcsFilesH')
    VCSRevContent -> (renderVcsRevH, renderVcsRevH')

renderHeaderBox :: MState -> Widget String
renderHeaderBox s = vLimit 2 $ vBox [middle, bottom]
  where
    (info, title) = renderHeaderWidget s
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
renderFileC s =
  case _list of
    Just lst -> renderList (const txt) False lst
    Nothing -> emptyContent
  where
    _list = (fContentList . file . curEntry) s

renderVcsFilesC :: MState -> Widget String
renderVcsFilesC st = renderList (const str) True _list
  where
    _list = (fileList . vcs) st

renderVcsRevC :: MState -> Widget String
renderVcsRevC st = renderList (const txt) True _list
  where
    _fp = fromJust $ selectedEntity st
    _vcsMap = vcsMapList . vcs $ st 
    _list = fromJust $ DHL.lookup _fp _vcsMap

emptyContent :: Widget String
emptyContent = vBox (lns ++ [fill ' '])
  where
    lns =
      map strWrap $
      lines
        "Sorry, but this file has not content\n\
        \Or it can't be readable by my tools"

renderContainer :: MState -> Widget String
renderContainer s =
  case mode s of
    DirContent -> renderCurDirC s
    FileContent -> renderFileC s
    VCSContent -> renderVcsFilesC s
    VCSRevContent -> renderVcsRevC s

draw :: MState -> Widget String
draw state = vBox [header, container]
  where
    header = renderHeaderBox state
    container = hBox [vBorder, renderContainer state, vBorder]

------------------------------------------------------------------------
-----------------------------Handle Events------------------------------
------------------------------------------------------------------------
handleEvent :: Event -> MState -> EventM String (Next MState)
handleEvent ev st =
  case mode st of
    VCSContent -> do
      op <- handleListEvent ev _list
      continue $ updateVcsList op st
      where _list = (fileList . vcs) st
    DirContent -> do
      op <- handleListEvent ev _list
      continue $ updateList op st
      where _list = (dEntryList . dir . curEntry) st
    FileContent ->
      case (fContentList . file . curEntry) st of
        Just a -> do
          op <- handleListEvent ev a
          continue $ updateFileList op st
        Nothing -> continue st
    VCSRevContent -> case _lst of
      Nothing -> continue $ st {action = DisplayError fileNotInVcs}
      Just a -> do
        op <- handleListEvent ev a
        continue $ st {vcs = newVcs op}
      where
          _fp = fromJust $ selectedEntity st
          _lstMap = (vcsMapList . vcs) st
          _lst = DHL.lookup _fp _lstMap
          newVcsMapList op = DHL.insert _fp op _lstMap
          newVcs op = (vcs st) {vcsMapList = newVcsMapList op}