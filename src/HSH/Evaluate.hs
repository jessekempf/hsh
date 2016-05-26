module HSH.Evaluate where

import HSH.CommandLineParse
import HSH.MonitoredDirectory

import Control.Monad
import Control.Monad.State
import System.IO
import System.Process

import qualified System.Environment as SysEnv
import qualified System.Posix.Directory as Posix

import qualified Data.Map as Map
import Data.List.Split
import Data.Maybe
import Data.Foldable

import GHC.IO.Exception (ExitCode(..))
import qualified Control.Exception as Exception

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
 - Command Evaluation
-}

-- | Evaluate a shell command abstract syntax tree.
evaluate :: ShellAST -> StateT ShellState IO ()
{- setenv -}
evaluate (SetEnv varname value) = do
  candidateNewState <- setEnv varname value <$> get

  newState <- if varname == "PATH"
                  then lift $ initialPathLoad candidateNewState
                  else return candidateNewState

  put newState

{- getenv -}
evaluate (GetEnv varname) = do
  env <- envVars <$> get
  lift $ putStrLn $ fromMaybe
                    ("Undefined environment variable '" ++ varname ++ "'.")
                    (Map.lookup varname env)

{- showstate -}
evaluate DebugState = get >>= lift . print

{- cd -}
evaluate (Chdir dir) = lift $ Posix.changeWorkingDirectory dir

{- Invoke external commands -}
evaluate (ExternalCommand command args) = do
  currentState <- get

  let procDesc = processDescriptor currentState command args

  spawnResult <- lift $ attemptSpawn currentState procDesc

  case spawnResult of
    (Left errorMessage) -> lift $ hPutStrLn stderr errorMessage
    (Right ph) -> do
        exitCode <- lift $ waitForProcess ph

        let numericExitCode = case exitCode of
                              ExitSuccess -> 0
                              ExitFailure num -> num

        put $ setEnv "?" (show numericExitCode) currentState

-- Compute a 'System.Process.CreateProcess' from the current shell state, a command name, and an
--  argv.
processDescriptor :: ShellState -> String -> [String] -> CreateProcess
processDescriptor currentState cmd =
  proc executable
  where
    executable = resolveExecutable currentState cmd

-- Attempt to spawn the described process. Will return a Right ProcessHandle on success, or Left
--  String error message on failure.
attemptSpawn :: ShellState -> CreateProcess -> IO (Either String ProcessHandle)
attemptSpawn currentState procDesc =
  Exception.handle
    (\(Exception.SomeException exc) -> return $ Left $ "Execution failed: " ++ show exc)
    (do
      (_i, _o, _e, ph) <- createProcess procDesc { env = Just $ Map.toList $ envVars currentState }
      return $ Right ph
    )
{- REPL Functions -}

replEvaluate :: ShellAST -> StateT ShellState IO ()
replEvaluate ast = do
  currentState <- get
  newState <- lift $ refreshPath currentState
  put newState

  evaluate ast

-- | Read, evaluate, and print a single command.
rep :: StateT ShellState IO ()
rep = do
  state <- get

  lift $ putStr $ shellPrompt state
  parseLine <$> lift getLine >>= replEvaluate

-- | Add looping to the REP.
repl :: StateT ShellState IO ()
repl = forever rep

-- | The 'clearEnv' function clears out the running process's ENV vars. This must be run
-- at shell start, otherwise 'System.Process.createProcess' will behave as if there is an
-- implicit merge between ShellState's envVars and the process's inherited `ENV`.
clearEnv :: IO ()
clearEnv = do
  vars <- map fst <$> SysEnv.getEnvironment
  traverse_ SysEnv.unsetEnv vars

-- | Run the Shell REPL.
runREPL :: IO ()
runREPL = do
  hSetBuffering stdout NoBuffering
  clearEnv

  initialState <- initialPathLoad defaultShellState

  evalStateT repl initialState
