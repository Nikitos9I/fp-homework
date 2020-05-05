{-# LANGUAGE LambdaCase #-}

module Manager where

import Brick (EventM, Next)
import Brick.Main (continue, suspendAndResume)
import Brick.Widgets.Edit (editorText)
import Brick.Widgets.List (GenericList, list, listSelectedElement)
import Control.Exception (SomeException, try)
import Data.HashMap.Lazy as DHL (delete, filterWithKey, insert, lookup)
import Data.Maybe (fromJust)
import Data.Text (pack)
import Debug.Trace
import Data.Time.Format (formatTime, defaultTimeLocale)
import IO
  ( Action(..)
  , Entity(..)
  , InfoDir(..)
  , InfoFile(..)
  , MState(..)
  , MapFPtoEntity
  , TextEditor
  )
import System.Process (callCommand)
import System.Directory (Permissions(..))

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
        Just (File info _) -> suspendAndResume $ runExternal (fPath info) state
        _ -> error "Unreacheble pattern"
    _ -> continue state

runExternal :: String -> MState -> IO MState
runExternal com s = do
  _ <- try $ callCommand ("nano " ++ com) :: IO (Either SomeException ())
  putStrLn "Done. Press ENTER to go back to file manager by Nikitos9I"
  _ <- getLine
  return s

goBack :: MState -> EventM String (Next MState)
-- handle exception with go back from root
goBack state =
  case currentEntity state of
    Dir _ fp _ -> continue $ openEntry' state fp
    File _ fp -> continue $ openEntry' state fp

initSearchTextEditor :: TextEditor
initSearchTextEditor = editorText "local" (Just 10) mempty

openSearch :: MState -> EventM String (Next MState)
openSearch state | trace ("I am here") False = undefined
openSearch state =
  continue $ state {action = Search initSearchTextEditor mempty}

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
          newInfo = (dir $ _parent p) {dEntryList = newGList newLst}
          newParent = (_parent p) {dir = newInfo, children = newLst}
          finalMap = DHL.insert (dPath newInfo) newParent newMap
      state {curEntry = fromJust $ DHL.lookup (dPath newInfo) finalMap, refMap = finalMap}
    File i p -> do
      let newMap = DHL.delete fp _map
          newLst =
            filter
              (\case
                 File info _ -> fPath info /= fPath i
                 _ -> True)
              (children $ _parent p)
          newInfo = (dir $ _parent p) {dEntryList = newGList newLst}
          newParent = (_parent p) {dir = newInfo, children = newLst}
          finalMap = DHL.insert (dPath newInfo) newParent newMap
      state {curEntry = fromJust $ DHL.lookup (dPath newInfo) finalMap, refMap = finalMap}
  where
    _map = refMap state
    entity = fromJust $ DHL.lookup fp _map
    _parent p = fromJust $ DHL.lookup p _map
    newGList newLst =
      list
        (fp ++ "1")
        (map
           (\case
              File info _ -> fPath info
              Dir info _ _ -> dPath info)
           newLst)
        1

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
  | sz > 1024 =
    show (round (fromIntegral sz / 1024 :: Double) :: Int) ++ "KB"
  | otherwise = show sz ++ "B"

displaySize :: Entity -> String
displaySize entry = case entry of
  Dir inf _ _ -> "Size: " ++ show (dSize inf) ++ " Bytes (" ++ renderSize (dSize inf) ++ ")"
  File inf _-> "Size: " ++ show (fSize inf) ++ " Bytes (" ++ renderSize (fSize inf) ++ ")"

displayPerms :: Entity -> [String]
displayPerms entry = case _perms of
  Nothing -> [" ", "Permissions unknown", "(could not read them)"]
  Just p -> [
      " ",
      "Is readable: " <> (if readable p then "yes" else "no"),
      "Is writable: " <> (if writable p then "yes" else "no"),
      "Is executable: " <> (if executable p then "yes" else "no"),
      "Is searchable: " <> (if searchable p then "yes" else "no")
    ]
  where
    _perms = case entry of
      Dir inf _ _ -> dPerms inf
      File inf _ -> fPerms inf

displayTimes :: Entity -> [String]
displayTimes entry = case _times of
  Nothing -> [" ", "Last access and modification times unknown", "(could not read them) or it is directory"]
  Just (acTm, mdTm) -> [" ", "Last access time:" <> format acTm, "Last modification time:" <> format mdTm]
  where
    format = formatTime defaultTimeLocale " %T %B %-d %Y"
    _times = case entry of
       Dir {} -> Nothing
       File inf _ -> fTimes inf

displayPath :: Entity -> String
displayPath entry = "Path: " ++ case entry of
  Dir i _ _ -> dPath i
  File i _ -> fPath i

displayMore :: Entity -> String
displayMore entry =
  case entry of
    Dir i _ _ -> "Number of files in current dir: " ++ show (dEntryListSize i)
    File i _ -> "File type: " ++ show (fType i)
    
displayInfo :: MState -> EventM String (Next MState)
displayInfo state =
  continue $ state {action = DisplayInfo entity}
  where
    _map = refMap state
    entity = fromJust $ DHL.lookup (fromJust $ selectedEntity state) _map