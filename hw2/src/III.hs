{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module III where

import Brick.Widgets.Edit (Editor)
import Brick.Widgets.List (GenericList, Splittable, list, splitAt)
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

type MapFPtoSize = HashMap FilePath Int

type MapFPtoNode = HashMap FilePath (FPTree FilePath)

type MapFPtoEntity = HashMap FilePath (FPTree Entity)

type TextEditor = Editor Text ()

newtype FMOptions =
  FMOptions
    { dirPath :: FilePath
    }

-- | Data structure for *file path tree*
-- Format:
--    File: filePath fileParentPath
--    Dir: dirPath dirParentPath [FPTree]
data FPTree a
  = FPFile a FilePath
  | FPDir a FilePath [FPTree a]
  deriving (Functor, Foldable, Show)

instance Traversable FPTree where
  traverse f (FPFile x y) = flip FPFile y <$> f x
  traverse f (FPDir x y l) =
    FPDir <$> f x <*> pure y <*> (traverse . traverse) f l

instance Splittable [] where
  splitAt n xs = (take n xs, drop n xs)

getFPTreeSize :: FPTree FilePath -> Int
getFPTreeSize (FPFile _ _) = 1
getFPTreeSize (FPDir _ _ l) = sum (Prelude.map getFPTreeSize l) + 1

data Action
  = Delete Entity
  | Mkdir TextEditor FilePath
  | Touch TextEditor FilePath
  | GoTo TextEditor
  | Search TextEditor FilePath
  | DisplayInfo Entity
  | DisplayError String
  | Nothing_
  
instance Show Action where
  show (Delete _) = " Delete "
  show (Mkdir _ _) = " Make Directory "
  show (Touch _ _) = " Touch File "
  show (GoTo _) = " Go To "
  show (DisplayInfo _) = " Entry Info "
  show (Search _ _) = " Search "
  show _ = " Error "

data MState =
  State
    { curEntry :: Entity
    , refMap :: MapFPtoEntity
    , entryTree :: FPTree Entity
    , search :: SearchInfo
    , action :: Action
    }

data SearchInfo
  = Empty
  | New
  | SearchInfo
      { searchQuery :: String
      , searchResult :: [Entity]
      }

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
    , dEntryList :: GenericList () [] (FPTree Entity)
    }
  deriving (Show)

instance Hashable InfoDir where
  hashWithSalt salt (InfoDir n s _ _ d _) = salt + hash n + hash s + hash d

data Entity
  = Dir
      { dir :: InfoDir
      }
  | File
      { file :: InfoFile
      }

instance Show Entity where
  show (Dir info) = show $ dName info
  show (File info) = show $ fName info

instance Eq Entity where
  Dir InfoDir {dPath = p1} == Dir InfoDir {dPath = p2} = p1 == p2
  File InfoFile {fPath = p1} == File InfoFile {fPath = p2} = p1 == p2
  _ == _ = False

instance Hashable Entity where
  hashWithSalt salt (Dir a) = salt + hash a
  hashWithSalt salt (File a) = salt + hash a

getRecursiveContents :: FilePath -> FilePath -> IO (FPTree FilePath)
getRecursiveContents topDir parent = do
  names <- getDirectoryContents topDir
  let properNames = filter (`notElem` [".", ".."]) names
  tree <-
    forM properNames $ \name -> do
      let path = topDir </> name
      isDir <- doesDirectoryExist path
      if isDir
        then getRecursiveContents path topDir
        else return $ FPFile path topDir
  return $ FPDir topDir parent tree

computeInitialSizes :: MapFPtoSize -> FPTree FilePath -> IO MapFPtoSize
computeInitialSizes _map (FPDir x _ l) = do
  treeSize <-
    forM l $ \case
      FPDir x' y' l' -> do
        nextTree <- computeInitialSizes _map (FPDir x' y' l')
        return (fromJust $ DHL.lookup x' nextTree, nextTree)
      FPFile x' y' -> do
        nextTree <- computeInitialSizes _map (FPFile x' y')
        return (fromJust $ DHL.lookup x' nextTree, nextTree)
  let dirSize = sum $ Prelude.map fst treeSize
  return $ insert x dirSize (Prelude.foldr (DHL.union . snd) DHL.empty treeSize)
computeInitialSizes _map (FPFile x _) = do
  _size <- getFileSize x
  return $ insert x (fromInteger _size) _map

computeFPtoNodeRefs :: MapFPtoNode -> FPTree FilePath -> IO MapFPtoNode
computeFPtoNodeRefs _map (FPFile x y) = return $ DHL.insert x (FPFile x y) _map
computeFPtoNodeRefs _map (FPDir x y l) = do
  let __map = DHL.insert x (FPDir x y l) _map
  maps <- forM l (computeFPtoNodeRefs __map)
  return $ Prelude.foldr DHL.union DHL.empty maps

computeFPtoNodeRefs' :: MapFPtoEntity -> FPTree Entity -> IO MapFPtoEntity
computeFPtoNodeRefs' _map (FPFile (File (InfoFile a b c d e f g)) y) =
  return $ DHL.insert f (FPFile (File (InfoFile a b c d e f g)) y) _map
computeFPtoNodeRefs' _map (FPDir (Dir (InfoDir a b c d e f)) y l) = do
  let __map = DHL.insert d (FPDir (Dir (InfoDir a b c d e f)) y l) _map
  maps <- forM l (computeFPtoNodeRefs' __map)
  return $ Prelude.foldr DHL.union DHL.empty maps
computeFPtoNodeRefs' _ _ = error "Incorrect pattern"

makeDirInfo :: MapFPtoSize -> MapFPtoNode -> FilePath -> IO InfoDir
makeDirInfo _map _refMap _dirPath = do
  let dirName = last (splitPath _dirPath) ++ "/"
      entryList = list () [] 1
  dirSize <- getSize _map _dirPath
  dirEntrySize <- getEntrySize _refMap _dirPath
  dirPerms <- toMaybe <$> try (getPermissions _dirPath)
  return $ InfoDir dirName dirSize dirPerms _dirPath dirEntrySize entryList

makeFileInfo :: MapFPtoSize -> FilePath -> IO InfoFile
makeFileInfo _map filePath = do
  let fileName = takeFileName filePath
      fileType = takeExtension filePath
  handle <- openFile filePath ReadMode
  content <- hGetContents handle
  fileSize <- getSize _map filePath
  filePerms <- toMaybe <$> try (getPermissions filePath)
  fileTimes <- toMaybe <$> try (getTimes filePath)
  return $
    InfoFile
      fileName
      (pack content)
      fileSize
      filePerms
      fileType
      filePath
      fileTimes

makeInfo :: MapFPtoSize -> MapFPtoNode -> FilePath -> IO Entity
makeInfo _map _refMap path = do
  isDir <- doesDirectoryExist path
  if isDir
    then Dir <$> makeDirInfo _map _refMap path
    else File <$> makeFileInfo _map path

toMaybe :: Either SomeException b -> Maybe b
toMaybe (Left _) = Nothing
toMaybe (Right x) = Just x

getSize :: MapFPtoSize -> FilePath -> IO Int
getSize _map path = return $ fromJust $ DHL.lookup path _map

getEntrySize :: MapFPtoNode -> FilePath -> IO Int
getEntrySize _refMap _filePath =
  return $ getFPTreeSize $ fromJust $ DHL.lookup _filePath _refMap

getTimes :: FilePath -> IO (UTCTime, UTCTime)
getTimes filePath = do
  accTime <- getAccessTime filePath
  modTime <- getModificationTime filePath
  return (accTime, modTime)

updateRenderingInfo :: MapFPtoEntity -> Entity -> IO Entity
updateRenderingInfo _map (Dir ent) =
  return $ Dir $ ent {dEntryList = list () lst 1}
  where
    (FPDir _ _ lst) = fromJust $ DHL.lookup (dPath ent) _map
updateRenderingInfo _map fl = return fl

initState :: FMOptions -> IO MState
initState opt = do
  let _dir = dirPath opt
  content <- getRecursiveContents _dir mempty
  _map <- computeInitialSizes DHL.empty content
  _refMap <- computeFPtoNodeRefs DHL.empty content
  entryContentTemp <- mapM (makeInfo _map _refMap) content
  __refMap <- computeFPtoNodeRefs' DHL.empty entryContentTemp
  entryContent <- mapM (updateRenderingInfo __refMap) entryContentTemp
  ___refMap <- computeFPtoNodeRefs' DHL.empty entryContent
  let (FPDir rootDirInfo _ _) = fromJust $ DHL.lookup _dir ___refMap
  print "State will init"
  return $ State rootDirInfo ___refMap entryContent Empty Nothing_
  