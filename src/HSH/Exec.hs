module HSH.Exec (runExternalCommand) where

import HSH.CommandLineParse
import HSH.ShellState

import System.IO
import System.Process

import qualified System.Posix.IO as Posix

import qualified Data.Map as Map

import Data.Maybe

import GHC.IO.Exception (ExitCode(..))
import qualified Control.Exception as Exception

runExternalCommand :: Command -> ShellState -> IO (ExitCode, [String])
runExternalCommand cmd currentState = do
  inputStream <- inputStreamFor cmd
  spawnResults <- runPipelineCommand inputStream cmd currentState []

  executionFailures <- catMaybes <$> mapM assess spawnResults

  case executionFailures of
    [] -> return (ExitSuccess, [])
    failures -> return (fst $ last failures, map snd failures)

  where
    inputStreamFor Command{ input = STDIN } = return $ UseHandle stdin
    inputStreamFor Command{ input = InputFile filename } = do
      handle <- openFile filename ReadMode
      return $ UseHandle handle

type SpawnResult = Either String ProcessHandle

assess :: SpawnResult -> IO (Maybe (ExitCode, String))
assess (Left errorMessage) = return $ Just (ExitFailure 127, errorMessage)
assess (Right processHandle) = waitForProcess processHandle >> return Nothing

runPipelineCommand :: StdStream -> Command -> ShellState -> [SpawnResult] -> IO [SpawnResult]

runPipelineCommand inputStream cmd@Command{ output = STDOUT } currentState resultsSoFar = do
  result <- pipelineSubprocess currentState cmd (inputStream, UseHandle stdout)
  return (result : resultsSoFar)

runPipelineCommand inputStream cmd@Command{ output = OutputFile filename } currentState resultsSoFar = do
  handle <- openFile filename WriteMode

  result <- pipelineSubprocess currentState cmd (inputStream, UseHandle handle)
  return (result : resultsSoFar)

runPipelineCommand inputStream cmd@Command{ output = DownstreamProcess dscmd } currentState resultsSoFar = do
  (readPipe, writePipe) <- makePipe

  downstreamResults <- runPipelineCommand readPipe dscmd currentState resultsSoFar

  result <- pipelineSubprocess currentState cmd (inputStream, writePipe)
  return (result : downstreamResults)

  where
    makePipe :: IO (StdStream, StdStream)
    makePipe = do
      (readFd, writeFd) <- Posix.createPipe

      readPipe <- pipeStreamFromFd readFd
      writePipe <- pipeStreamFromFd writeFd

      return (readPipe, writePipe)

      where
        pipeStreamFromFd fd = do
          Posix.setFdOption fd Posix.CloseOnExec True
          pipe <- Posix.fdToHandle fd
          hSetBuffering pipe NoBuffering
          return $ UseHandle pipe

pipelineSubprocess :: ShellState -> Command -> (StdStream, StdStream) -> IO SpawnResult
pipelineSubprocess currentState Command{ name = cmd, args = argv } (inputStream, outputStream) =
  Exception.handle
    (\(Exception.SomeException exc) -> return $ Left $ "Execution failed: " ++ show exc)
    (do
      (_i, _o, _e, ph) <- createProcess processDescriptor
      return $ Right ph
    )

  where
    processDescriptor =
      (proc executable argv){
        std_in = inputStream,
        std_out = outputStream,
        env = Just $ Map.toList $ envVars currentState
      }
      where
        executable = resolveExecutable currentState cmd
