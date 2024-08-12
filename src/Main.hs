module Main where

import Data.Text (pack, unpack)
import Data.Text.Lazy (toStrict)
import Parser (parseDef)
import System.Console.Haskeline
import Text.Pretty.Simple (pShow)

settings :: Settings IO
settings = defaultSettings {historyFile = Just ".miniml_history"}

repl :: InputT IO ()
repl = do
  input <- getMultilineInput ""
  case input of
    Just i -> case parseDef (pack i) of
      Left err -> outputStrLn $ "Error: " ++ unpack (toStrict $ pShow err)
      Right def -> outputStrLn $ unpack $ toStrict $ pShow def
    Nothing -> return ()
  repl

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
