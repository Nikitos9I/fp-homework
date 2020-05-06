{-# LANGUAGE LambdaCase #-}

module Manager where

import Brick (EventM, Next)
import Brick.Main (continue, suspendAndResume)
import Brick.Widgets.Edit (editorText)
import Brick.Widgets.List (GenericList, list, listSelectedElement)
import Control.Exception (SomeException, try)
import Data.HashMap.Lazy as DHL
  ( delete
  , empty
  , filterWithKey
  , insert
  , keys
  , lookup
  )
import Data.Maybe (fromJust)
import Data.Text (empty, isInfixOf, pack, Text)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Debug.Trace
import IO
  ( Action(..)
  , Entity(..)
  , InfoDir(..)
  , InfoFile(..)
  , MState(..)
  , MapFPtoEntity
  , TextEditor
  , VCS(..)
  )
import System.Process (callCommand)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (getCurrentTime)
import System.Directory.Internal (Permissions(..))
import System.FilePath.Posix ((</>), takeExtension)

data Content
  = DirContent
  | FileContent

getMode :: MState -> Content
getMode x =
  case curEntry x of
    Dir {} -> DirContent
    File _ _ -> FileContent

updateList :: GenericList String [] FilePath -> MState -> MState
updateList lst x = x {curEntry = entry}
  where
    entry = (curEntry x) {dir = infoDir}
    infoDir = ((dir . curEntry) x) {dEntryList = lst}
      
updateFileList :: GenericList String [] Text -> MState -> MState
updateFileList lst x = x {curEntry = entry}
  where
    entry = (curEntry x) {file = infoFile}
    infoFile = ((file . curEntry) x) {fContentList = lst}

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

openEntry' :: MState -> FilePath -> MState
openEntry' state _dirFP = state {curEntry = newEntry}
  where
    _map = refMap state
    newEntry = fromJust $ DHL.lookup _dirFP _map

openEntry :: MState -> EventM String (Next MState)
openEntry state =
  case selectedEntity state of
    Just fp ->
      case DHL.lookup fp (refMap state) of
        Just (Dir info _ _) -> continue $ openEntry' state (dPath info)
        Just (File info _) -> continue $ openEntry' state (fPath info)
        _ -> error "Unreacheble pattern"
    _ -> continue state

goBack :: MState -> EventM String (Next MState)
-- handle exception with go back from root
goBack state =
  case currentEntity state of
    Dir _ fp _ -> continue $ openEntry' state fp
    File _ fp -> continue $ openEntry' state fp

initTextEditor :: String -> TextEditor
initTextEditor suffix = editorText ("local" ++ suffix) (Just 3) mempty

openSearch :: MState -> EventM String (Next MState)
openSearch state =
  continue $ state {action = Search (initTextEditor "srf") mempty}

newGList :: [Entity] -> FilePath -> GenericList String [] FilePath
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
        }
  where
    _map = refMap state
    entity = fromJust $ DHL.lookup fp _map
    _parent p = fromJust $ DHL.lookup p _map

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
makeDirectory state =
  continue $ state {action = Mkdir (initTextEditor "mkd") mempty}

makeFile :: MState -> EventM String (Next MState)
makeFile state = continue $ state {action = Touch (initTextEditor "mkf") mempty}

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
          , fContentList = list "fContent" [] 1
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
  st {curEntry = newParentEnt, refMap = _newMap2}

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
        if _searchQuery == Data.Text.empty
          then DHL.empty
          else DHL.filterWithKey (\k _ -> _searchQuery `isInfixOf` pack k) _map
  DHL.keys _searchResult
  
openHelp :: MState -> EventM String (Next MState)
openHelp st = continue $ st {action = DisplayHelp} 

init :: MState -> EventM String (Next MState)
-- handle exception with init not from dir
init st = continue $ st {vcs = _vcs}
  where
    _vcs =
      VCS
        { rootFP = (dPath . dir . curEntry) st
        , vcsMapData = DHL.empty
        , vcsMapList = DHL.empty
        }

addEntity :: MState -> EventM String (Next MState)
-- handle exception with vcs initiated
addEntity st =
  continue $ st {action = VCSAdditionalComment (initTextEditor "ade") mempty}