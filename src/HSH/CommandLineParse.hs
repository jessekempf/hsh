module HSH.CommandLineParse where

import Data.List.Utils
import HSH.ShellState

import Debug.Trace

data ShellAST =
  SetEnv String String
  | GetEnv String
  | Chdir String
  | DebugState
  | ExternalCommand String [String]
  deriving (Eq, Show)

data EnvVarReference = EnvVarReference { begin :: Int, end :: Int } deriving (Eq, Show)

operationFromString :: String -> [String] -> ShellAST
operationFromString "setenv" [varname, value] = SetEnv varname value
operationFromString "getenv" [varname] = GetEnv varname
operationFromString "showstate" _ = DebugState
operationFromString "cd" [directory] = Chdir directory
operationFromString command args = ExternalCommand command args

expandVariables :: String -> ShellState -> Maybe String
expandVariables = maybeExpandVariableReferences . Just

expand :: [String] -> ShellState -> Maybe [String]
expand tokens shellstate = mapM expander tokens
  where
    expander = flip expandVariables shellstate

parseLine :: String -> ShellState -> Maybe ShellAST
parseLine commandLine shellstate = do
  tokens <- expand (words commandLine) shellstate
  Just $ operationFromString (head tokens) (tail tokens)

{-
-- Shell variable substitution code. This code is an absolute hack.
-}

{- Begin findNextVariableReference -}
-- Search for the first innermost variable reference and return it.
findNextVariableReference :: String -> (Char, String) -> Maybe String

-- Base cases: Either we run out of String to search, or we hit the first '}' in which case we
-- immediately return the EnvVarReference.
findNextVariableReference [] (_, _) = Nothing
findNextVariableReference ('}' : _) (_, reference) = Just $ "${" ++ reverse reference ++ "}"

-- We've just seen '${' so start a new variable reference
findNextVariableReference ('{' : rest ) ('$', _) =
  findNextVariableReference rest ('{', "")
-- Keep scanning through the string.
findNextVariableReference (char : rest) (x, reference) =
  findNextVariableReference rest (char, char : reference)
{- End findNextVariableReference -}

expandVariable :: String -> ShellState -> Maybe String
expandVariable ('$': '{' : str) shellstate =
  if last str == '}'
  then getEnv (init str) shellstate
  else Nothing
expandVariable str _ = Just str

maybeExpandVariableReferences ::  Maybe String -> ShellState -> Maybe String
maybeExpandVariableReferences Nothing _ = Nothing
maybeExpandVariableReferences (Just str) shellstate =
  case innermostVarReference of
    Just varname -> let expansion = expandVariable varname shellstate in
                    case expansion of
                      Just expn ->
                        let expandedString = replace varname expn str
                        in maybeExpandVariableReferences (Just expandedString) shellstate
                      Nothing -> Nothing
    Nothing -> Just str
  where
    innermostVarReference = findNextVariableReference str ('\0', "")
