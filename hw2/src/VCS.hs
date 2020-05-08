module VCS where

import Brick.Widgets.List (list)
import qualified Data.HashMap.Lazy as DHL
  ( insert
  , insertWith
  , keys
  , lookup
  , unions
  )
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text, append, pack)
import IO
  ( Action(..)
  , Entity(..)
  , InfoDir(..)
  , InfoFile(..)
  , MState(..)
  , VCS(..)
  )
import Manager (selectedEntity)

handleAddChangedFile :: MState -> FilePath -> Text -> MState
handleAddChangedFile st _fp comment = do
  let _refMap = refMap st
      _vcs = vcs st
      _vcsMap = vcsMapData _vcs
      _vcsList = vcsMapList _vcs
      _entity = fromJust $ DHL.lookup _fp _refMap
      _idx = length $ fromMaybe [] $ DHL.lookup _fp _vcsMap
      newVcsMap =
        DHL.insertWith (flip (++)) _fp [(_idx, _entity, comment)] _vcsMap
      _curList = fromJust $ DHL.lookup _fp newVcsMap
      _castedCurList =
        map
          (\(x, _, z) -> append (pack $ show x) (append (pack " ") z))
          _curList
      newVcsList = DHL.insert _fp (list "vcsList" _castedCurList 1) _vcsList
      newVcs = _vcs {vcsMapData = newVcsMap, vcsMapList = newVcsList}
  st {vcs = newVcs}

unionStates :: MState -> [MState] -> MState
unionStates x [] = x
unionStates x xs = x {vcs = newVcs}
  where
    vcsMapDatas = DHL.unions (map (vcsMapData . vcs) xs)
    vcsMapLists = DHL.unions (map (vcsMapList . vcs) xs)
    newVcs = (vcs x) {vcsMapData = vcsMapDatas, vcsMapList = vcsMapLists}

handleAddChangedEntity' :: MState -> Text -> Entity -> MState
handleAddChangedEntity' st com ent =
  case ent of
    File i _ -> handleAddChangedFile st (fPath i) com
    Dir _ _ lst -> unionStates st newStList
      where newStList = map (handleAddChangedEntity' st com) lst

handleAddChangedEntity :: MState -> Text -> MState
-- handle exception with add entity above root vcs dir
handleAddChangedEntity st comment = newState {action = Nothing_}
  where
    _fp = fromJust $ selectedEntity st
    _entity = fromJust $ DHL.lookup _fp (refMap st)
    tempState = handleAddChangedEntity' st comment _entity
    newVcsList =
      list "vcsGenericList" (DHL.keys $ (vcsMapData . vcs) tempState) 1
    newVcs = (vcs tempState) {fileList = newVcsList}
    newState = tempState {vcs = newVcs}