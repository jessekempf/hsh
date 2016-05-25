module HSH.MonitoredDirectory where

import Control.Monad

import Data.List
import qualified Data.Map as Map

import qualified System.Posix.Directory as Posix
import qualified System.Posix.Files as Posix
import qualified System.Posix.Types as Posix

import System.Directory
import System.FilePath
import System.IO

data MonitoredDirectory = MonitoredDirectory {
                            dirName :: FilePath,
                            lastModified :: Posix.EpochTime,
                            contents :: Map.Map FilePath QualifiedFilePath
                          } deriving (Eq, Show)

newtype QualifiedFilePath = QualifiedFilePath FilePath deriving Eq

instance Show QualifiedFilePath where
  show (QualifiedFilePath x) = show x

takeQualifiedFileName :: QualifiedFilePath -> FilePath
takeQualifiedFileName (QualifiedFilePath path) = takeFileName path

isVisible :: QualifiedFilePath -> Bool
isVisible qualpath = not $ "." `isPrefixOf` takeQualifiedFileName qualpath

onlyVisible :: [QualifiedFilePath] -> [QualifiedFilePath]
onlyVisible = filter isVisible

isCommand :: QualifiedFilePath -> IO Bool
isCommand (QualifiedFilePath path) = do
  stat <- Posix.getFileStatus path
  executable <- Posix.fileAccess path True False True

  return $ (Posix.isRegularFile stat || Posix.isSymbolicLink stat) && executable

qualifyPath :: FilePath -> FilePath -> QualifiedFilePath
qualifyPath dir = QualifiedFilePath . (dir </>)

listDirectoryQualified :: FilePath -> IO [QualifiedFilePath]
listDirectoryQualified dir = map (qualifyPath dir) <$> listDirectory dir

commandTableFrom :: [QualifiedFilePath] -> Map.Map FilePath QualifiedFilePath
commandTableFrom filenames = Map.fromList $ map (\name -> (takeQualifiedFileName name, name)) filenames

loadDirectory :: FilePath -> IO MonitoredDirectory
loadDirectory dir = do
  entries <- onlyVisible <$> listDirectoryQualified dir
  fileEntries <- filterM isCommand entries
  dirLastModified <- Posix.modificationTime <$> Posix.getFileStatus dir

  let entryMap = commandTableFrom fileEntries

  return MonitoredDirectory { dirName = dir, lastModified = dirLastModified, contents = entryMap }

refreshDirectory :: MonitoredDirectory -> IO MonitoredDirectory
refreshDirectory mondir = do
  dirLastModified <- Posix.modificationTime <$> Posix.getFileStatus (dirName mondir)

  if dirLastModified > lastModified mondir
  then
    loadDirectory $ dirName mondir
  else
    return mondir
