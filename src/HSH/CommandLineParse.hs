module HSH.CommandLineParse where

import Control.Monad.State

import Data.Maybe
import Data.List
import Data.List.Split
import Data.List.Utils

import HSH.ShellState

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
expandVariables str = maybeExpandVariableReferences (Just str)

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

data SearchMode = Continue | Halt

-- Search for the first innermost variable reference and return it.
findNextVariableReference :: String -> (Char, Int, SearchMode, Maybe EnvVarReference) -> Maybe EnvVarReference

-- Base cases: Either we run out of String to search, or we hit the first '}' in which case we
-- immediately return the EnvVarReference.
findNextVariableReference [] ('}', _, _, varRef) = varRef
findNextVariableReference [] (_, _, _, _) = Nothing
findNextVariableReference _ (_, _, Halt, varRef) = varRef

-- We've just seen '${' so make a note of it.
findNextVariableReference ('{' : rest ) ('$', currentOffset, _, _) =
  findNextVariableReference rest ('{', currentOffset + 1, Continue, Just (EnvVarReference currentOffset currentOffset))

-- We've seen our first '}', which means that we've reached the end of our first-innermost
-- variable reference.
findNextVariableReference ('}' : rest) (_, currentOffset, Continue, currentVarRef) =
  findNextVariableReference rest ('}', currentOffset + 1, Halt, Just (fromJust currentVarRef) { end = currentOffset + 1 })

-- Keep scanning through the string.
findNextVariableReference (char : rest) (previousChar, currentOffset, keepConsuming, currentVarRef) =
  findNextVariableReference rest (char, currentOffset + 1, Continue, currentVarRef)

variableName :: String -> EnvVarReference -> String
variableName str (EnvVarReference start end) = take (end - start + 1) $ drop (start - 1) str

expandVariable :: String -> ShellState -> Maybe String
expandVariable ('$': '{' : str) shellstate =
  if last str == '}'
  then getEnv (init str) shellstate
  else Nothing
expandVariable str _ = Just str

maybeExpandVariableReferences :: Maybe String -> ShellState -> Maybe String
maybeExpandVariableReferences Nothing _ = Nothing
maybeExpandVariableReferences (Just str) shellstate =
  case innermostVarReference of
    Just ref -> let varname = variableName str ref in
                let expansion = expandVariable varname shellstate in
                  case expansion of
                    Just exp -> maybeExpandVariableReferences (Just (replace varname exp str)) shellstate
                    Nothing -> Nothing
    Nothing -> Just str
  where
    innermostVarReference = findNextVariableReference str ('\0', 0, Continue, Nothing)
