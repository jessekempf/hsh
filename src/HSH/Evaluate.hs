module HSH.Evaluate where

import HSH.CommandLineParse

import Control.Monad.State
import System.IO

import qualified Data.Map as Map
import Data.Maybe

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


{- REPL Functions -}
rep :: StateT ShellState IO ()
rep = parseLine <$> lift getLine >>= evaluate

repl :: StateT ShellState IO ()
repl = forever rep

runREPL :: IO ()
runREPL = do
  hSetBuffering stdout NoBuffering
  evalStateT repl defaultShellState