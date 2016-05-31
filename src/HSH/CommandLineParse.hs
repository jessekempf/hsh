module HSH.CommandLineParse where

import Data.List.Utils
import HSH.ShellState

import Debug.Trace

data ShellAST =
  SetEnv String String
  | GetEnv String
  | Chdir String
  | DebugState
  | ShowParse ShellAST
  | External Command
  deriving (Eq, Show)

data EnvVarReference = EnvVarReference { begin :: Int, end :: Int } deriving (Eq, Show)

data Command = Command {
  name :: String,
  args :: [String],
  input :: InputDirection,
  output :: OutputDirection
} deriving (Eq, Show)

{-
-- Parse I/O Redirection Syntax
-}

redirectionAnalyzer :: ([String] -> [String] -> ([String], iodirection)) -> ([String] -> ([String], iodirection))
redirectionAnalyzer analyzer tokens =
  (reverse reversed_tokens, direction)
  where
    (reversed_tokens, direction) = analyzer tokens []

{- Derive an InputDirection and a list of non-input-related arguments from a list of tokens -}
data InputDirection = STDIN | InputFile String deriving (Eq, Show)

-- | Take a list of tokens and produce an OutputDirection and the tokens left over from building the
--    OutputDirection.
inputDirection :: [String] -> ([String], InputDirection)
inputDirection = redirectionAnalyzer _inputDirection

_inputDirection :: [String] -> [String] -> ([String], InputDirection)
_inputDirection [] args = (args, STDIN)
_inputDirection ("<" : filename : tokens) args = (reverse tokens ++ args, InputFile filename)
_inputDirection (('<' : filename) : tokens) args = _inputDirection ("<" : filename : tokens) args
_inputDirection (token : tokens) args = _inputDirection tokens (token : args)
{- END inputDirection -}

{- Derive an OutputDirection and a list of non-output-related arguments from a list of tokens -}
data OutputDirection = STDOUT | DownstreamProcess Command | OutputFile String deriving (Eq, Show)
-- | Take a list of tokens and produce an OutputDirection and the tokens left over from building the
--    OutputDirection.
outputDirection :: [String] -> ([String], OutputDirection)
outputDirection = redirectionAnalyzer _outputDirection

_outputDirection :: [String] -> [String] -> ([String], OutputDirection)
_outputDirection [] args = (args, STDOUT)
_outputDirection (">" : filename : tokens) args = (args, OutputFile filename)
_outputDirection (('>' : filename) : tokens) args = _outputDirection (">" : filename : tokens) args
_outputDirection ("|" : tokens) args = (args, DownstreamProcess $ commandFromString $ unwords tokens)
_outputDirection (('|' : cmd) : tokens) args = _outputDirection ("|" : cmd : tokens) args
_outputDirection (token : tokens) args = _outputDirection tokens (token : args)
{- END outputDirection -}

{- Build a process specification from a command string -}
commandFromString :: String -> Command
commandFromString string =
  Command cmd args input output
  where
    (cmd : tokens) = words string
    (inputless_args, input) = inputDirection tokens
    (args, output) = outputDirection inputless_args

{-
-- End of I/O Redirection Syntax Parsing
-}

operationFromString :: [String] -> ShellAST
operationFromString ["setenv", varname, value] = SetEnv varname value
operationFromString ["getenv", varname] = GetEnv varname
operationFromString ["showstate"] = DebugState
operationFromString ["cd", directory] = Chdir directory
operationFromString ("showparse" : tokens) = ShowParse $ operationFromString tokens
operationFromString tokens = External $ commandFromString $ unwords tokens

expandVariables :: String -> ShellState -> Maybe String
expandVariables = maybeExpandVariableReferences . Just

expand :: [String] -> ShellState -> Maybe [String]
expand tokens shellstate = mapM expander tokens
  where
    expander = flip expandVariables shellstate

parseLine :: String -> ShellState -> Maybe ShellAST
parseLine commandLine shellstate = do
  tokens <- expand (words commandLine) shellstate

  return $ operationFromString tokens

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
