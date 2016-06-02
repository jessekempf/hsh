module HSH.Evaluate where

import HSH.CommandLineParse
import HSH.ShellState
import HSH.Exec

import Control.Monad
import Control.Monad.State
import System.IO

import qualified System.Environment as SysEnv
import qualified System.Posix.Directory as Posix

import qualified Data.Map as Map

import Data.Maybe
import Data.Foldable

import GHC.IO.Exception (ExitCode(..))

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

{- showparse -}
evaluate (ShowParse ast) = lift $ print ast

{- cd -}
evaluate (Chdir dir) = lift $ Posix.changeWorkingDirectory dir

{- Invoke external commands -}
evaluate (External command) = do
  currentState <- get

  (exitCode, messages) <- lift $ runExternalCommand command currentState

  -- Report all execution errors
  mapM_ (lift . hPutStrLn stderr) messages

  -- Keep ${?} updated
  put $ setEnv "?" (numericString exitCode) currentState

  where
    numericString ExitSuccess = numericString (ExitFailure 0)
    numericString (ExitFailure num) = show num

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
  line <- lift getLine

  case parseLine line state of
    Just x -> replEvaluate x
    Nothing -> lift $ hPutStrLn stderr "Parsing input line failed."

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
