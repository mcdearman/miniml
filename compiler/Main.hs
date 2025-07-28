module Main where

import Control.Monad.State (evalState, runState)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Char as Char
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy.Encoding as TE
import Data.Void (Void)
import Error.Diagnose (addFile, defaultStyle, printDiagnostic, stderr)
import Error.Diagnose.Compat.Megaparsec (errorDiagnosticFromBundle)
import MMC.Common (InputMode (InputModeFile, InputModeInteractive))
import MMC.Pipeline
import MMC.Token (Token (..))
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

run :: String -> IO ()
run src = do
  let out = evalState (runPipeline (InputModeFile "main") (pack src)) defaultPipelineEnv
  putStrLn $ unpack . toStrict $ pShow out

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

-- parseRadix :: (Integral a) => a -> String -> a
-- parseRadix r = foldl' step 0
--   where
--     step a c = a * r + fromIntegral (Char.digitToInt c)

-- makeInt :: (Integral a) => a -> ByteString -> Token
-- makeInt 10 bs = (TokInt (parseRadix 10 (bsToText bs)))
-- makeInt 2 bs = (TokInt (parseRadix 2 (stripPrefix "0b" (bsToText bs))))
-- makeInt r _ = error "Unsupported radix" ++ show r

-- {-# INLINE bsToText #-}
-- bsToText :: ByteString -> T.Text
-- bsToText = TE.decodeUtf8 . BL.toStrict

-- {-# INLINE bsToString #-}
-- bsToString :: ByteString -> String
-- bsToString = T.unpack . bsToText

parseRadix :: (Integral a) => a -> Text -> a
parseRadix r = T.foldl' step 0
  where
    step a c = a * r + fromIntegral (Char.digitToInt c)