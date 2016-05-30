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

-- State machine to scan the input string. Its side effects include keeping an EnvVarReference
-- to the innermost-nested variable reference in the input string. For example, in ${FOO${BAR}}
-- its final state would include an EnvVarReference to `${BAR}`.
findNextVariableReference :: String -> State (Char, Int, Bool, Maybe EnvVarReference) ()
findNextVariableReference [] = return ()
findNextVariableReference (char : rest) = do
  (previousChar, currentOffset, keepConsuming, currentVarRef) <- get

  if previousChar == '$' && char == '{'
  then
    put (char, currentOffset + 1, True, Just (EnvVarReference currentOffset currentOffset))
  else
    if char == '}' && keepConsuming
    then
      put (char, currentOffset + 1, False, Just (fromJust currentVarRef) { end = currentOffset + 1 })
    else
      put (char, currentOffset + 1, True, currentVarRef)

  findNextVariableReference rest

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
  case innermostVarReferences of
    Just ref -> let varname = variableName str ref in
                let expansion = expandVariable varname shellstate in
                  case expansion of
                    Just exp -> maybeExpandVariableReferences (Just (replace varname exp str)) shellstate
                    Nothing -> Nothing
    Nothing -> Just str
  where
    (_, _, _, innermostVarReferences) = execState (findNextVariableReference str) ('\0', 0, True, Nothing)
