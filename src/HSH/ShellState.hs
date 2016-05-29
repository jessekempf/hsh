module HSH.ShellState where

import HSH.MonitoredDirectory

import qualified Data.Map as Map

import Data.List.Split
import Data.Maybe

data ShellState = ShellState {
                    envVars :: Map.Map EnvVarName EnvVarValue,
                    pathDirs :: [MonitoredDirectory]
                  } deriving (Eq, Show)

{-
-- ENV var manipulation
-}

type EnvVarName = String
type EnvVarValue = String

-- | Set an environment variable. Takes a name, value, and existing state and returns a
-- modified state.
setEnv :: EnvVarName -> EnvVarValue -> ShellState -> ShellState
setEnv name val shellstate =
  shellstate { envVars = Map.insert name val (envVars shellstate) }

-- | Get an environment variable. Takes a name and state and attempts to return the value
-- associated.
getEnv :: EnvVarName -> ShellState -> Maybe EnvVarValue
getEnv name ShellState{envVars = env} =  Map.lookup name env

-- | The default shell state.
defaultShellState :: ShellState
defaultShellState = ShellState {
    envVars = Map.fromList [("PROMPT", "haskell-sh $"), ("PATH", "/bin:/sbin")],
    pathDirs = []
  }

-- | Compute the shell prompt based on the current state.
shellPrompt :: ShellState -> String
shellPrompt ShellState{ envVars = env } =
  prompt ++ " "
  where
    prompt = fromMaybe
      "Prompt Undefined >"
      (Map.lookup "PROMPT" env)

resolveExecutable :: ShellState -> String -> String
resolveExecutable ShellState { pathDirs = [] } command = command
resolveExecutable currentState command =
  case listToMaybe $ mapMaybe (lookupCommand command) candidateDirs of
    Just (QualifiedFilePath x) -> x
    Nothing -> command
  where
    lookupCommand command mondir = Map.lookup command $ contents mondir
    candidateDirs = pathDirs currentState


{-
--
-- Impure path-expanding code
--
-}
initialPathLoad :: ShellState -> IO ShellState
initialPathLoad oldState = do
  newPathDirs <- mapM loadDirectory pathDirectories

  return oldState { pathDirs = newPathDirs }
  where
    pathDirectories = splitOn ":" path
    path = fromJust $ Map.lookup "PATH" $ envVars oldState

refreshPath :: ShellState -> IO ShellState
refreshPath oldState = do
  newPathDirs <- mapM refreshDirectory (pathDirs oldState)
  return oldState { pathDirs = newPathDirs }
