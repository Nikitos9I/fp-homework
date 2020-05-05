module Main where

import IO
import UI
import Options.Applicative
import System.Directory

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
         progDesc "A simple file manager with control version system usage")

runUI :: FMOptions -> IO ()
runUI options = do
  isDir <- doesDirectoryExist $ dirPath options
  _path <-
    if isDir
      then makeAbsolute $ dirPath options
      else return []
  case _path of
    [] -> putStrLn "Requires at least one dir path"
    _ -> do
        state <- initState options
        run state
--        eventChan <- Brick.BChan.newBChan 10
--        state <- Mngr.makeState path editComm eventChan (threadNum options)
--        void $ customMain buildVty (Just eventChan) (app atrm) state
