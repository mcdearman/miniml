module Main where

import Data.Text (pack)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

repl :: IO ()
repl = do
  hSetBuffering stdout NoBuffering
  putStr "> "
  input <- pack <$> getLine
  print input
  repl

main :: IO ()
main = do
  putStrLn "Welcome to the MiniML REPL!"
  repl
