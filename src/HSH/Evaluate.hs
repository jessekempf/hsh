module HSH.Evaluate where

import HSH.CommandLineParse

import Control.Monad.State
import System.IO
import System.Process

import qualified System.Environment as SysEnv

import qualified Data.Map as Map
import Data.Maybe
import Data.Foldable

import GHC.IO.Exception (ExitCode(..))

data ShellState = ShellState { envVars :: Map.Map String String } deriving (Eq, Show)

setEnv :: String -> String -> ShellState -> ShellState
setEnv name val shellstate =
  shellstate { envVars = Map.insert name val (envVars shellstate) }

defaultShellState :: ShellState
defaultShellState = ShellState { envVars = Map.empty }

{- The Evaluator -}
evaluate :: ShellAST -> StateT ShellState IO ()

evaluate (SetEnv varname value) = do
  newState <- setEnv varname value <$> get
  put newState

evaluate (GetEnv varname) = do
  env <- envVars <$> get
  lift $ putStrLn $ fromMaybe
                    ("Undefined environment variable '" ++ varname ++ "'.")
                    (Map.lookup varname env)

evaluate DebugState = get >>= lift . print

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
rep :: StateT ShellState IO ()
rep = parseLine <$> lift getLine >>= evaluate

repl :: StateT ShellState IO ()
repl = forever rep

clearEnv :: IO ()
clearEnv = do
  vars <- map fst <$> SysEnv.getEnvironment
  traverse_ SysEnv.unsetEnv vars

runREPL :: IO ()
runREPL = do
  hSetBuffering stdout NoBuffering
  clearEnv
  evalStateT repl defaultShellState
