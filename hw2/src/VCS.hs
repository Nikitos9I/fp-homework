module VCS where

import Brick.Widgets.List (list)
import qualified Data.HashMap.Lazy as DHL (insert, insertWith, lookup)
import Data.Maybe (fromJust)
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

handleAddEntity :: MState -> Text -> MState
handleAddEntity st comment = do
  let _refMap = refMap st
      _vcs = vcs st
      _vcsMap = vcsMapData _vcs
      _vcsList = vcsMapList _vcs
      _fp = fromJust $ selectedEntity st
      _entity = fromJust $ DHL.lookup _fp _refMap
      newVcsMap = DHL.insertWith (flip (++)) _fp [(_entity, comment)] _vcsMap
      _curList = fromJust $ DHL.lookup _fp _vcsMap
      _castedCurList =
        map
          (\(x, y) ->
             case x of
               Dir i _ _ -> append (pack $ dName i) y
               File i _ -> append (pack $ fName i) y)
          _curList
      newVcsList = DHL.insert _fp (list "vcsList" _castedCurList 1) _vcsList
      newVcs = _vcs {vcsMapData = newVcsMap, vcsMapList = newVcsList}
  st {action = Nothing_, vcs = newVcs}