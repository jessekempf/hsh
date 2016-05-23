module HSH.Evaluate where

import HSH.CommandLineParse

import Control.Monad.State
import System.IO

import qualified Data.Map as Map
import Data.Maybe

data ShellState = ShellState { envVars :: Map.Map String String } deriving (Eq, Show)

defaultShellState :: ShellState
defaultShellState = ShellState { envVars = Map.empty }

evaluate :: ShellAST -> StateT ShellState IO ()

evaluate (SetEnv varname value) = do
  shellState <- get
  put shellState { envVars = Map.insert varname value $ envVars shellState }

evaluate (GetEnv varname) = do
  env <- envVars <$> get
  lift $ putStrLn $ fromMaybe
                    ("Undefined environment variable '" ++ varname ++ "'.")
                    (Map.lookup varname env)

rep :: StateT ShellState IO ()
rep = parseLine <$> lift getLine >>= evaluate

repl :: StateT ShellState IO ()
repl = forever rep

runREPL :: IO ()
runREPL = do
  hSetBuffering stdout NoBuffering
  evalStateT repl defaultShellState
