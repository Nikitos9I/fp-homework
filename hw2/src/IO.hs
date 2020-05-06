{-# LANGUAGE FlexibleContexts #-}

module IO where

import Brick.Widgets.Edit (Editor)
import Brick.Widgets.List (GenericList, Splittable, list, splitAt)
import Conduit
import Control.Exception (SomeException, try)
import Control.Monad.State
import Data.HashMap.Lazy as DHL hiding (filter)
import Data.Hashable
import Data.Maybe
import Data.Text (Text, pack)
import Data.Time.Clock
import System.Directory
import System.FilePath ((</>), splitPath, takeExtension, takeFileName)
import System.IO (IOMode(..), hGetContents, openFile)

type MapFPtoEntity = HashMap FilePath Entity
type TextEditor = Editor Text String
type VCSMap = HashMap FilePath [(Entity, String)]
type VCSList = GenericList String [] String

newtype FMOptions =
  FMOptions
    { dirPath :: FilePath
    }

instance Splittable [] where
  splitAt n xs = (take n xs, drop n xs)

data MState =
  State
    { curEntry :: Entity
    , refMap :: MapFPtoEntity
    , action :: Action
    , vcs :: VCS
    }
    
data VCS = VCS {vcsMap :: VCSMap, vcsList :: VCSList} | VCSNothing

data Action
  = Mkdir { mEditor :: TextEditor, mPath :: FilePath }
  | Touch { tEditor :: TextEditor, tPath :: FilePath }
  | Search { sEditor :: TextEditor, sQuery :: Text }
  | DisplayInfo Entity
  | DisplayError String
  | Nothing_

instance Show Action where
  show (Mkdir _ _) = " Make Directory "
  show (Touch _ _) = " Touch File "
  show (DisplayInfo _) = " Entry Info "
  show (Search _ _) = " Search "
  show _ = " Error "

data InfoFile =
  InfoFile
    { fName :: String
    , fContent :: Text
    , fSize :: Int
    , fPerms :: Maybe Permissions
    , fType :: String
    , fPath :: FilePath
    , fTimes :: Maybe (UTCTime, UTCTime)
    }
  deriving (Show)

instance Hashable InfoFile where
  hashWithSalt salt (InfoFile n _ s _ _ p _) = salt + hash n + hash s + hash p

data InfoDir =
  InfoDir
    { dName :: String
    , dSize :: Int
    , dPerms :: Maybe Permissions
    , dPath :: FilePath
    , dEntryListSize :: Int
    , dEntryList :: GenericList String [] FilePath
    }
  deriving (Show)

instance Hashable InfoDir where
  hashWithSalt salt (InfoDir n s _ _ d _) = salt + hash n + hash s + hash d

data Entity
  = Dir
      { dir :: InfoDir
      , parent :: FilePath
      , children :: [Entity]
      }
  | File
      { file :: InfoFile
      , parent :: FilePath
      }

instance Show Entity where
  show (Dir info _ _) = show (dName info)
  show (File info _) = show (fName info)

instance Eq Entity where
  Dir InfoDir {dPath = p1} _ _ == Dir InfoDir {dPath = p2} _ _ = p1 == p2
  File InfoFile {fPath = p1} _ == File InfoFile {fPath = p2} _ = p1 == p2
  _ == _ = False

instance Hashable Entity where
  hashWithSalt salt (Dir a _ _) = salt + hash a
  hashWithSalt salt (File a _) = salt + hash a

toMaybe :: Either SomeException b -> Maybe b
toMaybe (Left _) = Nothing
toMaybe (Right x) = Just x

getTimes :: FilePath -> IO (UTCTime, UTCTime)
getTimes filePath = do
  accTime <- getAccessTime filePath
  modTime <- getModificationTime filePath
  return (accTime, modTime)

makeDirInfo :: FilePath -> IO InfoDir
makeDirInfo _dirPath = do
  let dirName = last (splitPath _dirPath) ++ "/"
  dirSize <-
    runConduitRes $
    sourceDirectoryDeep False _dirPath .| mapMC (liftIO . getFileSize) .| sumC
  dirEntryList <- runConduitRes $ sourceDirectory _dirPath .| sinkList
  dirPerms <- toMaybe <$> try (getPermissions _dirPath)
  let entryList = list _dirPath dirEntryList 1
  return $
    InfoDir
      dirName
      (fromInteger dirSize)
      dirPerms
      _dirPath
      (length dirEntryList)
      entryList

makeFileInfo :: FilePath -> IO InfoFile
makeFileInfo filePath = do
  let fileName = takeFileName filePath
      fileType = takeExtension filePath
  handle <- openFile filePath ReadMode
  content <- hGetContents handle
  fileSize <- getFileSize filePath
  filePerms <- toMaybe <$> try (getPermissions filePath)
  fileTimes <- toMaybe <$> try (getTimes filePath)
  return $
    InfoFile
      fileName
      (pack content)
      (fromInteger fileSize)
      filePerms
      fileType
      filePath
      fileTimes

getRecursiveContents :: FilePath -> FilePath -> IO MapFPtoEntity
getRecursiveContents topDir _parent = do
  names <- getDirectoryContents topDir
  let properNames = filter (`notElem` [".", ".."]) names
  tree <-
    forM properNames $ \name -> do
      let path = topDir </> name
      isDir <- doesDirectoryExist path
      if isDir
        then getRecursiveContents path topDir
        else do
          infoFile <- makeFileInfo path
          return $ DHL.insert path (File infoFile topDir) DHL.empty
  let _union = DHL.unions tree
      _children =
        Prelude.map
          (fromJust . flip DHL.lookup _union . (<>) (topDir ++ "/"))
          properNames
  topInfo <- makeDirInfo topDir
  return $ DHL.insert topDir (Dir topInfo _parent _children) _union

initState :: FMOptions -> IO MState
initState opt = do
  let _dir = dirPath opt
  _content <- getRecursiveContents _dir mempty
  let root = fromJust $ DHL.lookup _dir _content
  return $ State root _content Nothing_ VCSNothing

testOpt :: FMOptions
testOpt = FMOptions {dirPath = "/Users/nikita.savinov/Downloads/hw_ot_Nikitosa"}