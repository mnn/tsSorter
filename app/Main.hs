module Main where

import           System.Hclip

import           Lib

main :: IO ()
main = do
  input <- getClipboard
  putStrLn $ "Input:\n" ++ input ++ "\n"
  case processData input of
    Left err -> putStrLn err
    Right (DataProcessingResult x log) -> do
      putStrLn $ "Log:\n" ++ log ++ "\n"
      putStrLn $ "Result:\n" ++ x
      setClipboard x
