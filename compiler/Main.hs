module Main where

import Data.ByteString (ByteString)
import Data.Text (pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)
import MMC.Build.Driver (runDriver)
import MMC.Build.Effect (defaultPipelineEnv)
import MMC.Build.Pipeline
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

run :: ByteString -> IO ()
run src = do
  env <- defaultPipelineEnv src
  out <- runDriver env src
  putStrLn . unpack . toStrict $ pShow out

-- let (out, _) = runState (runPipeline (InputModeFile "main") (pack src)) defaultPipelineEnv
-- case out of
--   Left e ->
--     let diag = errorDiagnosticFromBundle Nothing ("Parse error on input" :: Text) Nothing e
--         diag' = addFile diag "main" src
--      in printDiagnostic stderr True True 2 defaultStyle diag'
--   Right prog -> putStrLn $ unpack . toStrict $ pShow prog

main :: IO ()
main = run "x = match y with\n  1 -> True\n  2 -> False"

-- putStrLn "Welcome to the miniML REPL!"
-- runInputT settings (repl defaultPipelineEnv)
