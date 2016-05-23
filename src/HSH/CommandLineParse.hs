module HSH.CommandLineParse where

data ShellAST =
  SetEnv String String
  | GetEnv String
  | ExternalCommand String [String]
  deriving (Eq, Show)

operationFromString :: String -> [String] -> ShellAST
operationFromString "setenv" [varname, value] = SetEnv varname value
operationFromString "getenv" [varname] = GetEnv varname
operationFromString command args = ExternalCommand command args

parseLine :: String -> ShellAST
parseLine commandLine =
  operationFromString (head tokens) (tail tokens)
  where
    tokens = words commandLine
