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
    --package lens
    --package rainbow
    --package lens-simple
    --package lens-family
    --package lens-family-core
    --package lens-family-th
-}

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

import           Control.Arrow       ((>>>))
import           Control.Lens
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.State
import qualified Data.Text           as T
import           Filesystem.Path     (replaceExtension)
import           Prelude             hiding (FilePath)
import qualified Rainbow             as RB
import           System.Hclip
import           Turtle              hiding (count)

data TestState = TestState
  { _testStateTestPath    :: FilePath
  , _testStateProjectPath :: FilePath
  } deriving (Show, Eq)

makeFields ''TestState

type TestStateMonad a = StateT TestState a

leftPad :: Int -> a -> [a] -> [a]
leftPad m c xs = replicate (m - length ys) c ++ ys where ys = take m xs

testFileNames :: [String]
testFileNames = [1..] & map (\x->"test." ++ leftPad 2 '0' (show x) ++ ".ts")

io :: IO a -> TestStateMonad IO a
io = liftIO

consolePrefix :: String
consolePrefix = "DoTests: "

rawPr :: String -> IO ()
rawPr x = putStrLn $ consolePrefix ++ x

pr :: String -> TestStateMonad IO ()
pr = io . rawPr

printChunksLn :: (RB.Renderable a) => [RB.Chunk a] -> TestStateMonad IO ()
printChunksLn chunks = do
  mapM_ (io . RB.putChunk) chunks
  io $ putStrLn ""

genChunk :: String -> RB.Chunk RB.ByteString
genChunk x = RB.chunk (fromString x :: RB.ByteString)

printChunksLnWithPrefix :: [RB.Chunk RB.ByteString] -> TestStateMonad IO ()
printChunksLnWithPrefix xs = printChunksLn $ p:xs where
  p = genChunk consolePrefix & RB.fore RB.grey

liftedCd :: FilePath -> TestStateMonad IO ()
liftedCd = io . cd

dropBlankLines :: String -> String
dropBlankLines = lines >>> filter (/="") >>> unlines

stripLastEnter :: String -> String
stripLastEnter [] = []
stripLastEnter x  = if last x == '\n' then init x else x

doTest :: String -> TestStateMonad IO Bool
doTest fileName  = do
  let testFile = fromString fileName
  let testOutputFile = replaceExtension testFile "out.ts"
  testBody <- io $ readTextFile testFile
  io $ setClipboard $ T.unpack testBody
  use projectPath >>= liftedCd
  (testExitCode, testStdOut, testStdErr) <- shellStrictWithErr "stack exec ts-sorter-exe" ""
  use testPath >>= liftedCd
  case testExitCode of
    ExitFailure n -> die $ "Test run failed. " <> repr n <> " StdErr:\n" <> testStdErr
    ExitSuccess   -> do
      modifiedTestBody <- io getClipboard
      io $ writeTextFile testOutputFile (fromString modifiedTestBody)
      -- TODO
      (lintExitCode, lintStdOut, lintStdErr) <- shellStrictWithErr (fromString $ "node_modules/.bin/tslint " ++ show testOutputFile) ""
      case lintExitCode of
        ExitFailure n -> do
          io $ putStrLn $ "TSLint run failed. Exit code = " ++ show lintExitCode
          io $ putStrLn $ "processed text:\n" ++ modifiedTestBody
          io $ putStrLn "\nTSLint output:"
          printLog lintStdOut
          printLog lintStdErr
          return False
        ExitSuccess   -> return True
  where
  printLog x = do
    let x' = x & T.unpack & dropBlankLines & stripLastEnter
    when (x' /= "") $ io $ putStrLn x'
    return ()

doTestWrapped :: String -> TestStateMonad IO Bool
doTestWrapped fileName = do
  let startChunk = genChunk "[start] " & RB.fore RB.blue
  printChunksLnWithPrefix [startChunk, genChunk fileName]
  res <- doTest fileName
  let endChunk = genChunk "[end] " & RB.fore RB.cyan
  let resChunk = if res then genChunk "Success" & RB.fore RB.green
                        else genChunk "Failure" & RB.fore RB.red & RB.bold
  printChunksLnWithPrefix [endChunk, genChunk fileName, genChunk " - ", resChunk, genChunk "\n"]
  return res

runTests :: TestStateMonad IO ()
runTests = do
  existingTestFiles <- takeWhileM (io . fileFilter) testFileNames
  pr $ "test files = " ++ show existingTestFiles
  results <- mapM doTestWrapped existingTestFiles
  io $ putStrLn ""
  pr " results"
  pr "~-~-~-~-~"
  pr $ "total tests = " ++ show (length results)
  pr $ "succeeded   = " ++ show (count id results)
  let failedCount = count not results
  let failedChunk = let chunk = genChunk (show failedCount) in
                      if failedCount > 0 then chunk & RB.fore RB.red & RB.bold
                                         else chunk
  printChunksLnWithPrefix [genChunk "failed      = ", failedChunk]
  where
    fileFilter :: String -> IO Bool
    fileFilter = testfile . fromString

tsLintBinPath :: String
tsLintBinPath = "node_modules/.bin/tslint"

tsLintConfig :: String
tsLintConfig = "tslint.json"

envChecks :: IO ()
envChecks = do
  tsLintFound <- testfile $ fromString tsLintBinPath
  unless tsLintFound $ error "TSLint binary is missing. Perhaps you forgot to run \"npm install\" in test directory?"
  tsLintConfigFound <- testfile $ fromString tsLintConfig
  unless tsLintConfigFound $ error "TSLint config is missing."

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

main :: IO ()
main = do
  envChecks

  myPath <- pwd >>= realpath
  projectPath <- realpath $ myPath </> ".."

  cd projectPath
  buildRes <- shell "stack build" empty
  case buildRes of
    ExitSuccess   -> return ()
    ExitFailure n -> die $ "Build failed. " <> repr n
  rawPr "build succeeded"
  cd myPath

  let initialState = TestState {
      _testStateTestPath = myPath
    , _testStateProjectPath = projectPath
  }
  execStateT runTests initialState
  return ()
