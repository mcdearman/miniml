module Main where

import Control.Monad.State (runState)
import Data.Text (pack, unpack)
import MMC.Common (InputMode (InputModeFile, InputModeInteractive))
import MMC.Pipeline
import System.Console.Haskeline

settings :: Settings IO
settings = defaultSettings {historyFile = Just ".repl_history"}

repl :: PipelineEnv -> InputT IO ()
repl env = do
  input <- getMultilineInput ""
  -- let r = Compiler.resolver c
  -- let s = Compiler.solver c
  -- let ctx = Solver.ctx s
  -- let cs = Solver.constraints s
  -- let sub = Solver.subst s
  case input of
    -- Just "env" -> do
    --   outputStrLn $ unpack $ (toStrict . pShow) r
    --   repl c
    -- Just "res" -> do
    --   outputStrLn $ unpack $ (toStrict . pShow) ()
    --   repl c
    -- Just "ctx" -> do
    --   outputStrLn $ unpack $ (toStrict . pShow) ctx
    --   repl c
    -- Just "constraints" -> do
    --   outputStrLn $ unpack $ pretty cs
    --   repl c
    -- Just "sub" -> do
    --   outputStrLn $ unpack $ (toStrict . pShow) sub
    --   repl c
    Just src -> do
      let (out, env') = runState (runPipeline InputModeInteractive (pack src)) env
      outputStrLn $ unpack out
      repl env'
    Nothing -> return ()

getMultilineInput :: String -> InputT IO (Maybe String)
getMultilineInput acc = do
  firstLine <- getInputLine "> "
  case firstLine of
    Nothing -> pure Nothing
    Just fl -> collectLines (acc ++ fl ++ "\n")

collectLines :: String -> InputT IO (Maybe String)
collectLines acc = do
  minput <- getInputLine ""
  case minput of
    Nothing -> pure Nothing
    Just "" -> pure $ Just (init acc)
    Just input -> collectLines (acc ++ input ++ "\n")

main :: IO ()
main = do
  -- putStrLn "Welcome to the miniML REPL!"
  -- runInputT settings (repl defaultPipelineEnv)
  let src = "x = 1"
  let (out, _) = runState (runPipeline (InputModeFile "test.mml") (pack src)) defaultPipelineEnv
  putStrLn $ unpack out
