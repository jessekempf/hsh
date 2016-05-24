module HSH.Evaluate where

import HSH.CommandLineParse

import Control.Monad.State
import System.IO
import System.Process

import qualified System.Environment as SysEnv
import qualified System.Posix.Directory as Posix

import qualified Data.Map as Map
import Data.Maybe
import Data.Foldable

import GHC.IO.Exception (ExitCode(..))

data ShellState = ShellState { envVars :: Map.Map EnvVarName EnvVarValue } deriving (Eq, Show)

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
defaultShellState = ShellState { envVars = Map.singleton "PROMPT" "haskell-sh $" }

-- | Compute the shell prompt based on the current state.
shellPrompt :: ShellState -> String
shellPrompt ShellState{ envVars = env } =
  prompt ++ " "
  where
    prompt = fromMaybe
      "Prompt Undefined >"
      (Map.lookup "PROMPT" env)

{-
 - Command Evaluation
-}

-- | Evaluate a shell command abstract syntax tree.
evaluate :: ShellAST -> StateT ShellState IO ()

{- setenv -}
evaluate (SetEnv varname value) = do
  newState <- setEnv varname value <$> get
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

  (_i, _o, _e, ph) <- lift $ createProcess (proc command args) { env = Just $ Map.toList $ envVars currentState }

  exitCode <- lift $ waitForProcess ph

  let numericExitCode = case exitCode of
                          ExitSuccess -> 0
                          ExitFailure num -> num

  let newState = setEnv "?" (show numericExitCode) currentState

  put newState

{- REPL Functions -}

-- | Read, evaluate, and print a single command.
rep :: StateT ShellState IO ()
rep = do
  state <- get

  lift $ putStr $ shellPrompt state
  parseLine <$> lift getLine >>= evaluate

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
  evalStateT repl defaultShellState
