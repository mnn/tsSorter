#!/usr/bin/env stack
{- stack --resolver ghc-8.0.2 --install-ghc
    runghc
    --package unix-compat
    --package system-filepath
    --package system-fileio
    --package optional-args
    --package mwc-random
    --package managed
    --package hostname
    --package foldl
    --package turtle
    --package monad-loops
    --package Hclip
-}

{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Control.Monad.Loops
import qualified Data.Text           as T
import           System.Hclip
import           Turtle

leftPad :: Int -> a -> [a] -> [a]
leftPad m c xs = replicate (m - length ys) c ++ ys where ys = take m xs

testFileNames :: [String]
testFileNames = [1..] & map (\x->"test." ++ leftPad 2 '0' (show x) ++ ".ts")

pr :: String -> IO ()
pr x = putStrLn $ "DoTests: " ++ x

doTest :: String -> IO Bool
doTest fileName = do
  pr $ "[start] " ++ fileName
  testBody <- readTextFile (fromString fileName)
  setClipboard $ T.unpack testBody
  shellStrictWithErr "stack" ["exec", "ts-sorter-exe"]
  modifiedTestBody <- getClipboard
  -- TODO
  pr $ "modifiedTestBody = " ++ modifiedTestBody
  -- procStrictWithErr
  pr $ "[ end ] " ++ fileName
  return True

runTests = do
  existingTestFiles <- takeWhileM fileFilter testFileNames
  pr $ "test files = " ++ show existingTestFiles
  results <- mapM doTest existingTestFiles
  pr $ "results = " ++ show results
  where
    fileFilter :: String -> IO Bool
    fileFilter f = testfile $ fromString f

main :: IO ()
main = do
  myPath <- pwd
  let projectPath = myPath </> ".."

  cd projectPath
  buildRes <- shell "stack build" empty
  case buildRes of
    ExitSuccess   -> return ()
    ExitFailure n -> die $ "Build failed. " <> repr n
  pr "build succeeded"
  cd myPath

  cd projectPath
  runTests
  cd myPath

-- node_modules/.bin/tslint test.01.ts
-- xclip -selection clipboard -i res/test.02.ts && ./run.sh
