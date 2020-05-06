module Main where

import IO (FMOptions(..), initState)
import Options.Applicative
  ( Parser
  , (<**>)
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , metavar
  , progDesc
  , short
  , showDefault
  , strOption
  , value
  )
import System.Directory (doesDirectoryExist, makeAbsolute)
import UI (run)

opts :: Parser FMOptions
opts =
  FMOptions <$>
  strOption
    (long "dirpath" <> short 'd' <> metavar "FILEPATH" <>
     help "Directory to open" <>
     showDefault <>
     value ".")

fmp :: FMOptions
fmp = FMOptions {dirPath = "/Users/nikita.savinov/Downloads/hw_ot_Nikitosa"}

main :: IO ()
main = runUI =<< execParser options
  where
    options =
      info
        (opts <**> helper)
        (fullDesc <> header "Smart File Manager With VCS" <>
         progDesc
           "A simple file manager with control version system under the hood")

runUI :: FMOptions -> IO ()
runUI options = do
  isDir <- doesDirectoryExist $ dirPath options
  _path <-
    if isDir
      then makeAbsolute $ dirPath options
      else return []
  case _path of
    [] -> putStrLn "Requires dir path"
    _ -> do
      state <- initState options
      run state