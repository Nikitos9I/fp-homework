{-# LANGUAGE LambdaCase #-}

module Manager where

import Brick (EventM, Next)
import Brick.Main (continue)
import Brick.Widgets.Edit (editorText)
import Brick.Widgets.List (GenericList, list, listRemove, listSelectedElement)
import Data.HashMap.Lazy as DHL
  ( delete
  , empty
  , filterWithKey
  , insert
  , keys
  , lookup
  )
import Data.Maybe (fromJust)
import Data.Text as T (Text, empty, isInfixOf, pack)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Errors
  ( cdAboveRootDir
  , fileNotInVcs
  , initVCSFromFile
  , openVCSListFromFile
  , vcsAlreadyExist
  , vcsNotInitiated
  )
import IO
  ( Action(..)
  , ContentMode(..)
  , Entity(..)
  , InfoDir(..)
  , InfoFile(..)
  , MGList
  , MState(..)
  , MapFPtoEntity
  , TextEditor
  , VCS(..)
  )
import System.Directory.Internal (Permissions(..))
import System.FilePath.Posix ((</>), takeExtension)

updateList :: MGList -> MState -> MState
updateList lst x = x {curEntry = entry}
  where
    entry = (curEntry x) {dir = infoDir}
    infoDir = ((dir . curEntry) x) {dEntryList = lst}

updateFileList :: GenericList String [] Text -> MState -> MState
updateFileList lst x = x {curEntry = entry}
  where
    entry = (curEntry x) {file = infoFile}
    infoFile = ((file . curEntry) x) {fContentList = Just lst}

updateVcsList :: MGList -> MState -> MState
updateVcsList lst x = x {vcs = _vcs}
  where
    _vcs = (vcs x) {fileList = lst}

selectedEntity :: MState -> Maybe FilePath
selectedEntity x =
  case curEntry x of
    Dir info _ _ ->
      Just $ snd $ fromJust $ listSelectedElement $ dEntryList info
    File _ _ -> Nothing

currentEntity :: MState -> Entity
currentEntity x =
  case curEntry x of
    Dir info _ _ -> fromJust $ DHL.lookup (dPath info) (refMap x)
    File info _ -> fromJust $ DHL.lookup (fPath info) (refMap x)

getMode :: Entity -> ContentMode
getMode ent =
  case ent of
    Dir {} -> DirContent
    File {} -> FileContent

openEntry' :: MState -> FilePath -> MState
openEntry' state _dirFP =
  case DHL.lookup _dirFP _map of
    Just newEntry -> state {curEntry = newEntry, mode = getMode newEntry}
    Nothing -> state {action = DisplayError cdAboveRootDir}
  where
    _map = refMap state

openEntry :: MState -> EventM String (Next MState)
openEntry state =
  case selectedEntity state of
    Just fp ->
      case DHL.lookup fp (refMap state) of
        Just (Dir info _ _) -> continue $ openEntry' state (dPath info)
        Just (File info _) -> continue $ openEntry' state (fPath info)
        _ -> error "Unreacheble pattern"
    _ -> continue state

actionWithEntry :: MState -> EventM String (Next MState)
actionWithEntry state =
  case mode state of
    VCSContent -> continue $ handleRemoveVcsFile state
    VCSRevContent -> continue $ handleRemoveVcsRevFile state
    _ -> openEntry state

goBack :: MState -> EventM String (Next MState)
goBack state =
  case mode state of
    DirContent -> continue $ openEntry' state $ (dParent . curEntry) state
    FileContent -> continue $ openEntry' state $ (fParent . curEntry) state
    VCSContent -> continue $ state {mode = DirContent}
    VCSRevContent -> continue $ state {mode = DirContent}

initTextEditor :: String -> TextEditor
initTextEditor suffix = editorText ("local" ++ suffix) (Just 3) mempty

openSearch :: MState -> EventM String (Next MState)
openSearch state =
  continue $ state {action = Search (initTextEditor "srf") mempty}

newGList :: [Entity] -> FilePath -> MGList
newGList newLst fp =
  list
    (fp ++ "1")
    (map
       (\case
          File info _ -> fPath info
          Dir info _ _ -> dPath info)
       newLst)
    1

deleteRecursive :: [Entity] -> MapFPtoEntity -> MapFPtoEntity
deleteRecursive lst =
  filterWithKey
    (\k _ ->
       k `notElem`
       map
         (\case
            Dir info _ _ -> dPath info
            File info _ -> fPath info)
         lst)

deleteEntity' :: FilePath -> MState -> MState
deleteEntity' fp state =
  case entity of
    Dir i p l -> do
      let newMap = deleteRecursive l $ DHL.delete fp _map
          newLst =
            filter
              (\case
                 Dir info _ _ -> dPath info /= dPath i
                 _ -> True)
              (children $ _parent p)
          newInfo = (dir $ _parent p) {dEntryList = newGList newLst fp}
          newParent = (_parent p) {dir = newInfo, children = newLst}
          finalMap = DHL.insert (dPath newInfo) newParent newMap
      state
        { curEntry = fromJust $ DHL.lookup (dPath newInfo) finalMap
        , refMap = finalMap
        }
    File i p -> do
      let newMap = DHL.delete fp _map
          newLst =
            filter
              (\case
                 File info _ -> fPath info /= fPath i
                 _ -> True)
              (children $ _parent p)
          newInfo = (dir $ _parent p) {dEntryList = newGList newLst fp}
          newParent = (_parent p) {dir = newInfo, children = newLst}
          finalMap = DHL.insert (dPath newInfo) newParent newMap
      state
        { curEntry = fromJust $ DHL.lookup (dPath newInfo) finalMap
        , refMap = finalMap
        , toDelete = _toDelete
        }
  where
    _map = refMap state
    entity = fromJust $ DHL.lookup fp _map
    _parent p = fromJust $ DHL.lookup p _map
    _toDelete = DHL.insert fp entity $ toDelete state

deleteEntity :: MState -> EventM String (Next MState)
deleteEntity state =
  case selectedEntity state of
    Just fp -> continue $ deleteEntity' fp state
    _ -> continue state

renderSize :: Int -> String
renderSize sz
  | sz > 1073741824 =
    show (round (fromIntegral sz / 1073741824 :: Double) :: Int) ++ "GB"
  | sz > 1048576 =
    show (round (fromIntegral sz / 1048576 :: Double) :: Int) ++ "MB"
  | sz > 1024 = show (round (fromIntegral sz / 1024 :: Double) :: Int) ++ "KB"
  | otherwise = show sz ++ "B"

displaySize :: Entity -> String
displaySize entry =
  case entry of
    Dir inf _ _ ->
      "Size: " ++
      show (dSize inf) ++ " Bytes (" ++ renderSize (dSize inf) ++ ")"
    File inf _ ->
      "Size: " ++
      show (fSize inf) ++ " Bytes (" ++ renderSize (fSize inf) ++ ")"

displayPerms :: Entity -> [String]
displayPerms entry =
  case _perms of
    Nothing -> [" ", "Permissions unknown", "(could not read them)"]
    Just p ->
      [ " "
      , "Is readable: " <>
        (if readable p
           then "yes"
           else "no")
      , "Is writable: " <>
        (if writable p
           then "yes"
           else "no")
      , "Is executable: " <>
        (if executable p
           then "yes"
           else "no")
      , "Is searchable: " <>
        (if searchable p
           then "yes"
           else "no")
      ]
  where
    _perms =
      case entry of
        Dir inf _ _ -> dPerms inf
        File inf _ -> fPerms inf

displayTimes :: Entity -> [String]
displayTimes entry =
  case _times of
    Nothing ->
      [ " "
      , "Last access and modification times unknown"
      , "(could not read them) or it is directory"
      ]
    Just (acTm, mdTm) ->
      [ " "
      , "Last access time:" <> format acTm
      , "Last modification time:" <> format mdTm
      ]
  where
    format = formatTime defaultTimeLocale " %T %B %-d %Y"
    _times =
      case entry of
        Dir {} -> Nothing
        File inf _ -> fTimes inf

displayPath :: Entity -> String
displayPath entry =
  "Path: " ++
  case entry of
    Dir i _ _ -> dPath i
    File i _ -> fPath i

displayMore :: Entity -> String
displayMore entry =
  case entry of
    Dir i _ _ -> "Number of files in current dir: " ++ show (dEntryListSize i)
    File i _ -> "File type: " ++ show (fType i)

displayInfo :: MState -> EventM String (Next MState)
displayInfo state = continue $ state {action = DisplayInfo entity}
  where
    _map = refMap state
    entity = fromJust $ DHL.lookup (fromJust $ selectedEntity state) _map

makeDirectory :: MState -> EventM String (Next MState)
makeDirectory state = continue $ state {action = Mkdir (initTextEditor "mkd")}

makeFile :: MState -> EventM String (Next MState)
makeFile state = continue $ state {action = Touch (initTextEditor "mkf")}

writeToFile :: MState -> EventM String (Next MState)
writeToFile state =
  continue $
  state
    {action = Write (initTextEditor "wtf") (fromJust $ selectedEntity state)}

defaultPermissions :: Permissions
defaultPermissions =
  Permissions
    {readable = True, writable = True, executable = False, searchable = True}

makeNewDir :: MState -> String -> Entity
makeNewDir st _dirName = do
  let _dirPath = (dPath . dir . curEntry) st </> _dirName
      _perms = defaultPermissions
      _dir =
        InfoDir
          { dName = _dirName ++ "/"
          , dSize = 0
          , dPerms = Just _perms
          , dPath = _dirPath
          , dEntryListSize = 0
          , dEntryList = list _dirPath [] 1
          }
  Dir _dir ((dPath . dir . curEntry) st) []

makeNewFile :: MState -> String -> IO Entity
makeNewFile st _fileName = do
  let _filePath = (dPath . dir . currentEntity) st </> _fileName
      _perms = defaultPermissions
  _times <- getCurrentTime
  let _file =
        InfoFile
          { fName = _fileName
          , fContent = mempty
          , fSize = 0
          , fPerms = Just _perms
          , fType = takeExtension _filePath
          , fPath = _filePath
          , fTimes = Just (_times, _times)
          , fContentList = Just $ list "fContent" [] 1
          }
  return $ File _file ((dPath . dir . curEntry) st)

insertNew :: MState -> Entity -> MState
insertNew st ent = do
  let _map = refMap st
      newList = (children . curEntry) st ++ [ent]
      newParent =
        ((dir . curEntry) st)
          {dEntryList = newGList newList ((dPath . dir . curEntry) st)}
      newParentEnt = (curEntry st) {dir = newParent}
      _newMap1 = DHL.insert (dPath newParent) newParentEnt _map
      _newMap2 =
        case ent of
          Dir i _ _ -> DHL.insert (dPath i) ent _newMap1
          File i _ -> DHL.insert (fPath i) ent _newMap1
      _toInsert =
        case ent of
          Dir i _ _ -> DHL.insert (dPath i) ent (toInsert st)
          File i _ -> DHL.insert (fPath i) ent (toInsert st)
  st {curEntry = newParentEnt, refMap = _newMap2, toInsert = _toInsert}

handleMakeDirEvent :: MState -> String -> MState
handleMakeDirEvent st _dirName =
  (insertNew st (makeNewDir st _dirName)) {action = Nothing_}

handleTouchEvent :: MState -> String -> IO MState
handleTouchEvent st _fileName = do
  _file <- makeNewFile st _fileName
  return $ (insertNew st _file) {action = Nothing_}

handleSearchEvent :: MState -> [FilePath]
handleSearchEvent st = do
  let _searchQuery = (sQuery . action) st
      _map = refMap st
      _searchResult =
        if _searchQuery == T.empty
          then DHL.empty
          else DHL.filterWithKey (\k _ -> _searchQuery `isInfixOf` pack k) _map
  DHL.keys _searchResult

handleWriteToFile :: MState -> Text -> FilePath -> IO MState
handleWriteToFile st _txt fp = do
  let _map = refMap st
      _ent = fromJust $ DHL.lookup fp _map
      _lst = list "fContentNew" [_txt] 1
      updatedFile = (file _ent) {fContent = Just _txt, fContentList = Just _lst}
      newMap = DHL.insert fp (_ent {file = updatedFile}) _map
      newUpdated = DHL.insert fp (_ent {file = updatedFile}) (toUpdate st)
  return $ st {refMap = newMap, action = Nothing_, toUpdate = newUpdated}

openHelp :: MState -> EventM String (Next MState)
openHelp st = continue $ st {action = DisplayHelp}

init :: MState -> EventM String (Next MState)
init st =
  case vcs st of
    VCS {} -> continue $ st {action = DisplayError vcsAlreadyExist}
    _ ->
      case curEntry st of
        File {} -> continue $ st {action = DisplayError initVCSFromFile}
        Dir {} -> continue $ st {vcs = _vcs}
  where
    _list = list "vcsGenericList" [] 1
    _vcs =
      VCS
        { rootFP = (dPath . dir . curEntry) st
        , fileList = _list
        , vcsMapData = DHL.empty
        , vcsMapList = DHL.empty
        }

addChangedEntity :: MState -> EventM String (Next MState)
addChangedEntity st =
  case vcs st of
    VCSNothing -> continue $ st {action = DisplayError vcsNotInitiated}
    _ ->
      continue $
      st {action = VCSAdditionalComment (initTextEditor "ade") mempty}

openVCSList :: MState -> EventM String (Next MState)
openVCSList st =
  case vcs st of
    VCSNothing -> continue $ st {action = DisplayError vcsNotInitiated}
    _ ->
      case curEntry st of
        File {} -> continue $ st {action = DisplayError openVCSListFromFile}
        Dir {} -> continue $ st {mode = VCSContent}

selectedVcsFile :: MGList -> FilePath
selectedVcsFile lst = snd $ fromJust $ listSelectedElement lst

handleRemoveVcsFile :: MState -> MState
handleRemoveVcsFile st = do
  let _vcs = vcs st
      _selected = selectedVcsFile $ fileList _vcs
      _data = DHL.delete _selected $ vcsMapData _vcs
      _list = DHL.delete _selected $ vcsMapList _vcs
      newList = list "newVcsFileList" (DHL.keys _data) 1
      newVcs = _vcs {fileList = newList, vcsMapData = _data, vcsMapList = _list}
  st {vcs = newVcs}

openVCSRevList :: MState -> EventM String (Next MState)
openVCSRevList st =
  case vcs st of
    VCSNothing -> continue $ st {action = DisplayError vcsNotInitiated}
    _ ->
      case curEntry st of
        File {} -> continue $ st {action = DisplayError openVCSListFromFile}
        Dir {} ->
          case _lst of
            Nothing -> continue $ st {action = DisplayError fileNotInVcs}
            _ -> continue $ st {mode = VCSRevContent}
          where _fp = fromJust $ selectedEntity st
                _lstMap = (vcsMapList . vcs) st
                _lst = DHL.lookup _fp _lstMap

handleRemoveVcsRevFile :: MState -> MState
handleRemoveVcsRevFile st = do
  let _vcs = vcs st
      _mapList = vcsMapList _vcs
      _fp = fromJust $ selectedEntity st
      _lst = fromJust $ DHL.lookup _fp _mapList
      _selected = fromJust $ listSelectedElement _lst
      newLst = listRemove (fst _selected) _lst
      newMapLst = DHL.insert _fp newLst _mapList
      _mapData = vcsMapData _vcs
      _lstD = fromJust $ DHL.lookup _fp _mapData
      newLstD = filter (\(x, _, _) -> x /= fst _selected) _lstD
      newMapData = DHL.insert _fp newLstD _mapData
      newVcs = _vcs {vcsMapList = newMapLst, vcsMapData = newMapData}
  st {vcs = newVcs}