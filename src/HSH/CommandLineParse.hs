module HSH.CommandLineParse where

import Control.Monad.State
import qualified Data.Map as Map

data ShellState = ShellState { envVars :: Map.Map String String }

data Operation =
  SetEnv String String
  | GetEnv String
  | ExternalCommand String [String]
  deriving (Eq, Show)

operationFromString :: String -> [String] -> Operation
operationFromString "setenv" [varname, value] = SetEnv varname value
operationFromString "getenv" [varname] = GetEnv varname
operationFromString command args = ExternalCommand command args

parseLine :: String -> Operation
parseLine commandLine =
  operationFromString (head tokens) (tail tokens)
  where
    tokens = words commandLine
