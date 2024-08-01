module Main where

import System.Console.Haskeline

settings :: Settings IO
settings = defaultSettings {historyFile = Just ".miniml-history"}

repl :: InputT IO ()
repl = do
  minput <- getInputLine "> "
  case minput of
    Nothing -> return ()
    Just "quit" -> return ()
    Just input -> do outputStrLn $ "Input was: " ++ input; repl

main :: IO ()
main = do
  putStrLn "Welcome to the MiniML REPL!"
  runInputT settings repl
