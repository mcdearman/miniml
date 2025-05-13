module Main where

import MMC.Common
import System.Console.Haskeline
import Text.Pretty.Simple (pShow)

settings :: Settings IO
settings = defaultSettings {historyFile = Just ".repl_history"}

-- repl :: Compiler -> InputT IO ()
-- repl c = do
--   input <- getMultilineInput ""
--   let r = Compiler.resolver c
--   let s = Compiler.solver c
--   let ctx = Solver.ctx s
--   let cs = Solver.constraints s
--   let sub = Solver.subst s
--   case input of
--     Just "env" -> do
--       outputStrLn $ unpack $ (toStrict . pShow) r
--       repl c
--     Just "res" -> do
--       outputStrLn $ unpack $ (toStrict . pShow) ()
--       repl c
--     Just "ctx" -> do
--       outputStrLn $ unpack $ (toStrict . pShow) ctx
--       repl c
--     Just "constraints" -> do
--       outputStrLn $ unpack $ pretty cs
--       repl c
--     Just "sub" -> do
--       outputStrLn $ unpack $ (toStrict . pShow) sub
--       repl c
--     Just src -> do
--       let (out, c') = runState (Compiler.run (pack src)) c
--       outputStrLn $ unpack out
--       repl c'
--     Nothing -> return ()

getMultilineInput :: String -> InputT IO (Maybe String)
getMultilineInput acc = do
  firstLine <- getInputLine "> "
  case firstLine of
    Nothing -> return Nothing
    Just fl -> collectLines (acc ++ fl ++ "\n")

collectLines :: String -> InputT IO (Maybe String)
collectLines acc = do
  minput <- getInputLine ""
  case minput of
    Nothing -> return Nothing
    Just "" -> return $ Just (init acc)
    Just input -> collectLines (acc ++ input ++ "\n")

main :: IO ()
main = do
  putStrLn "Welcome to the MiniML REPL!"

-- runInputT settings (repl Compiler.defaultCompiler)

take :: Int -> [a] -> [a]
take n ls = if 0 < n then unsafeTake n ls else []
  where
    -- A version of take that takes the whole list if given an argument
    -- less than 1
    unsafeTake :: Int -> [a] -> [a]
    unsafeTake !_ [] = []
    unsafeTake 1 (x : _) = [x]
    unsafeTake m (x : xs) = x : unsafeTake (m - 1) xs