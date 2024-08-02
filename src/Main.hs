module Main where

import System.Console.Haskeline

settings :: Settings IO
settings = defaultSettings {historyFile = Just ".miniml-history", autoAddHistory = False}

repl :: InputT IO ()
repl = do
  input <- getMultilineInput ""
  case input of
    Just i -> do
      outputStrLn $ "You entered:\n" ++ i
      putHistory i
    Nothing -> return ()
  repl -- Continue the loop

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
  runInputT settings repl
