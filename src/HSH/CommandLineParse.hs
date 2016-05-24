module HSH.CommandLineParse where

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

parseLine :: String -> ShellAST
parseLine commandLine =
  operationFromString (head tokens) (tail tokens)
  where
    tokens = words commandLine
