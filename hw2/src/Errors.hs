module Errors
  ( cdAboveRootDir
  , openVCSListFromFile
  , initVCSFromFile
  , vcsNotInitiated
  , fileNotInVcs
  , vcsAlreadyExist
  ) where

type Exception = String

cdAboveRootDir :: Exception
cdAboveRootDir = "Sorry, You can't go above the root directory"

openVCSListFromFile :: Exception
openVCSListFromFile = "Sorry, You can't open vcs files list in read file mode"

initVCSFromFile :: Exception
initVCSFromFile = "Sorry, You can't init vcs from file"

vcsNotInitiated :: Exception
vcsNotInitiated = "Sorry, You should init vcs firstly"

fileNotInVcs :: Exception
fileNotInVcs = "Sorry, this file is not under vcs"

vcsAlreadyExist :: Exception
vcsAlreadyExist = "Sorry, vcs already exists"