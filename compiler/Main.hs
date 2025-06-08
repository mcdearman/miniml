{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Control.Monad.State (runState)
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy (toStrict)
import Data.Void (Void)
import Error.Diagnose (defaultStyle, printDiagnostic, stderr)
import Error.Diagnose.Compat.Megaparsec (HasHints (..), errorDiagnosticFromBundle)
import Error.Diagnose.Diagnostic (prettyDiagnostic)
import MMC.Common (InputMode (InputModeFile, InputModeInteractive))
import MMC.Pipeline
import System.Console.Haskeline
import Text.Pretty.Simple (pShow)

settings :: Settings IO
settings = defaultSettings {historyFile = Just ".repl_history"}

-- repl :: PipelineEnv -> InputT IO ()
-- repl env = do
--   input <- getMultilineInput ""
--   -- let r = Compiler.resolver c
--   -- let s = Compiler.solver c
--   -- let ctx = Solver.ctx s
--   -- let cs = Solver.constraints s
--   -- let sub = Solver.subst s
--   case input of
--     -- Just "env" -> do
--     --   outputStrLn $ unpack $ (toStrict . pShow) r
--     --   repl c
--     -- Just "res" -> do
--     --   outputStrLn $ unpack $ (toStrict . pShow) ()
--     --   repl c
--     -- Just "ctx" -> do
--     --   outputStrLn $ unpack $ (toStrict . pShow) ctx
--     --   repl c
--     -- Just "constraints" -> do
--     --   outputStrLn $ unpack $ pretty cs
--     --   repl c
--     -- Just "sub" -> do
--     --   outputStrLn $ unpack $ (toStrict . pShow) sub
--     --   repl c
--     Just src -> do
--       let (out, env') = runState (runPipeline InputModeInteractive (pack src)) env
--       outputStrLn $ unpack out
--       repl env'
--     Nothing -> return ()

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
  let (out, _) = runState (runPipeline (InputModeFile "main") (pack src)) defaultPipelineEnv
  case out of
    Left e ->
      let diag = errorDiagnosticFromBundle Nothing ("Parse error on input" :: Text) Nothing e
       in printDiagnostic stderr True True 2 defaultStyle diag
    Right prog -> putStrLn $ unpack (toStrict (pShow prog))

instance HasHints Void msg where
  hints _ = mempty