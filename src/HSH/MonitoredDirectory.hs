module HSH.MonitoredDirectory where

import Control.Monad

import Data.List
import qualified Data.Map as Map

import qualified System.Posix.Directory as Posix
import qualified System.Posix.Files as Posix
import qualified System.Posix.Types as Posix

import qualified System.Directory as Dir

import System.FilePath
import System.IO

data MonitoredDirectory = MonitoredDirectory {
                            dirName :: FilePath,
                            lastModified :: Posix.EpochTime,
                            contents :: Map.Map FilePath QualifiedFilePath
                          } deriving (Eq, Show)

type FileName = FilePath

newtype QualifiedFilePath = QualifiedFilePath FilePath deriving Eq
newtype DirectoryPath = DirectoryPath FilePath

instance Show QualifiedFilePath where
  show (QualifiedFilePath x) = show x

takeQualifiedFileName :: QualifiedFilePath -> FilePath
takeQualifiedFileName (QualifiedFilePath path) = takeFileName path

-- | Indicate whether a path references a hidden file.
isVisible :: QualifiedFilePath -> Bool
isVisible qualpath = not $ "." `isPrefixOf` takeQualifiedFileName qualpath

-- | Filter out hidden files from a list.
onlyVisible :: [QualifiedFilePath] -> [QualifiedFilePath]
onlyVisible = filter isVisible

-- | Determine whether the given path refers to an executable unix command.
isCommand :: QualifiedFilePath -> IO Bool
isCommand (QualifiedFilePath path) = do
  stat <- Posix.getFileStatus path
  executable <- Posix.fileAccess path True False True

  return $ (Posix.isRegularFile stat || Posix.isSymbolicLink stat) && executable

-- | Qualify a path to a file by prepending the directory it's in.
qualifyPath :: DirectoryPath -> FilePath -> QualifiedFilePath
qualifyPath (DirectoryPath dir) = QualifiedFilePath . (dir </>)

-- | Do 'System.Directory.listDirectory' to a DirectoryPath.
listDirectory :: DirectoryPath -> IO [FilePath]
listDirectory (DirectoryPath dir) = Dir.listDirectory dir

-- | List the contents of a directory, each prefixed with the name of the directory.
listDirectoryQualified :: DirectoryPath -> IO [QualifiedFilePath]
listDirectoryQualified dir = map (qualifyPath dir) <$> listDirectory dir

-- | Create a command table mapping command names to executable paths based on a list of
--    qualified file paths.
commandTableFrom :: [QualifiedFilePath] -> Map.Map FilePath QualifiedFilePath
commandTableFrom filenames = Map.fromList $ map (\name -> (takeQualifiedFileName name, name)) filenames

-- | Create a MonitoredDirectory for a given path.
--
--    XXX: This function does no validation tha the FilePath points to a directory.
loadDirectory :: FilePath -> IO MonitoredDirectory
loadDirectory dir = do
  entries <- onlyVisible <$> listDirectoryQualified (DirectoryPath dir)
  fileEntries <- filterM isCommand entries
  dirLastModified <- Posix.modificationTime <$> Posix.getFileStatus dir

  let entryMap = commandTableFrom fileEntries

  return MonitoredDirectory { dirName = dir, lastModified = dirLastModified, contents = entryMap }

-- | Ensure the MonitoredDirectory reflects what is on-disk.
refreshDirectory :: MonitoredDirectory -> IO MonitoredDirectory
refreshDirectory mondir = do
  dirLastModified <- Posix.modificationTime <$> Posix.getFileStatus (dirName mondir)

  if dirLastModified > lastModified mondir
  then
    loadDirectory $ dirName mondir
  else
    return mondir
