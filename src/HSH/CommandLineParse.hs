module HSH.CommandLineParse where

import Data.Maybe

import HSH.ShellState

data ShellAST =
  SetEnv String String
  | GetEnv String
  | Chdir String
  | DebugState
  | ExternalCommand String [String]
  deriving (Eq, Show)

operationFromString :: String -> [String] -> ShellAST
operationFromString "setenv" [varname, value] = SetEnv varname value
operationFromString "getenv" [varname] = GetEnv varname
operationFromString "showstate" _ = DebugState
operationFromString "cd" [directory] = Chdir directory
operationFromString command args = ExternalCommand command args

tokenExpand :: String -> ShellState -> Maybe String
tokenExpand ('$': '{' : str) shellstate =
  if last str == '}'
  then getEnv (init str) shellstate
  else Nothing
tokenExpand str _ = Just str

expand :: [String] -> ShellState -> Maybe [String]
expand tokens shellstate = mapM expander tokens
  where
    expander = flip tokenExpand shellstate

parseLine :: String -> ShellState -> Maybe ShellAST
parseLine commandLine shellstate = do
  tokens <- expand (words commandLine) shellstate
  Just $ operationFromString (head tokens) (tail tokens)
