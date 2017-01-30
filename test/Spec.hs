{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Arrow
import           Control.Lens.Operators        ((&))
import           Data.Either                   (isLeft)
import           Data.Maybe                    (isJust)
import           Lib
import           Test.Framework
import           Text.ParserCombinators.Parsec hiding (spaces)

test_test = assertEqual True True

posNegCase :: Parser a -> [String -> IO ()]
posNegCase parser = [positive, negative]
  where
    fullParser = parser >> eof
    parseIt = parse fullParser ""
    positive str = assertEqual (Right ()) (parseIt str)
    negative str = assertBool (parseIt str & isLeft)

testParser :: Parser a -> [String] -> [String] -> IO ()
testParser parser posStr negStr = do
  let [positive, negative] = posNegCase parser
  mapM_ positive posStr
  mapM_ negative negStr

test_anyNewLine = do
  let pos = ["\n", "\r\n"]
  let neg = ["\r", "", " "]
  testParser anyNewLine pos neg

test_parseImportGroupAlias = do
  let pos = ["abc", "Abc"]
  let neg = ["-", " ", ""]
  testParser parseImportGroupAlias pos neg

test_parseImportSourceObjectName = do
  let pos = ["abc", "Abc"]
  let neg = ["-", " ", ""]
  testParser parseImportSourceObjectName pos neg

test_parseImportSourceObjectNameWithStar = do
  let pos = ["*", "abc", "Abc"]
  let neg = ["**", "-", ""]
  testParser parseImportSourceObjectNameWithStar pos neg

test_parseAliasPart = do
  let pos = ["as Q", "as  Aaa"]
  let neg = ["as", "as ", " as Q", "as Q  "]
  testParser parseAliasPart pos neg

test_parseImportPart = do
  let pos = ["A  as  B", "* as Aaa", "Ccc as Ddd", "Q", "Qqq"]
  let neg = ["A as", "as X", " * as A", "a "]
  testParser (parseImportPart True) pos neg

test_parseAliasPartPrefixedWithSpaces = do
  let pos = [" as W", "as W", "   as Qqq"]
  let neg = ["Q", "Y as X"]
  testParser parseAliasPartPrefixedWithSpaces pos neg

test_parseSimpleImport = do
  let pos = ["* as A", "a", "A as B"]
  let neg = ["", "as Q", " * as Q", "q ", " q"]
  testParser parseSimpleImport pos neg

test_parseGroupImportDelim = do
  let pos = [",", " , "]
  let neg = ["", " "]
  testParser parseGroupImportDelim pos neg

test_parseRawGroupImportParts = do
  let pos = ["A", "A,B", "A , B", "A ,B", "A, B"]
  let neg = [" A", "A ", "A,B ", "", ",", " , "]
  testParser parseRawGroupImportParts pos neg

test_parseGroupImport = do
  let pos = ["{A}", "{ A}", "{A }", "{A as B}", "{A as B }", "{ Aaa as B }", "{A, B, C as D}"]
  let neg = ["", "{", "{}", "{ }", "{* as C}"]
  testParser parseGroupImport pos neg

test_parseFileName = do
  let pos = ["./abc", "./a/b/c", "@angular", "../x-y"]
  let neg = ["", " ", "a a"]
  testParser parseFileName pos neg

test_parseFileNameString = do
  let pos = ["'./abc'", "\"@a-b\"", "'@'"]
  let neg = ["'x\"", "\"xyz", "@'", "", "@"]
  testParser parseFileNameString pos neg

test_parseImport = do
  let pos = [ "import { S } from '../../app-sanctuary';"
            , "import ConfigDefault from '../../config-default';"
            , "import { createTextSchema, createTextsResponseSchema } from './data/texts-classes';"
            , "import * as Joi from 'joi-browser';"
            , "import { Injectable } from '@angular/core';"
            ]
  let neg = [""]
  testParser parseImport pos neg

test_parseCodeGroup = do
  let pos = ["import ConfigDefault from '../../config-default';\n\
             \import * as Joi from 'joi-browser';"
            , "import { Injectable } from '@angular/core';\n\
              \import { createLanguageSchema, createLanguagesResponseSchema } from './data/languages-classes';\n\
              \import {\n\
              \   createJobFunctionSchema,\n\
              \   createJobFunctionsResponseSchema\n\
              \ } from './data/job-functions-classes';"
            ]
  let neg = ["import * from 'X';\n\nimport a from 'Y';"]
  testParser parseCodeGroup pos neg

test_parseTop = do
  let pos = ["import { Injectable } from '@angular/core';\n\
             \\n\
             \import { createLanguageSchema, createLanguagesResponseSchema } from './data/languages-classes';\n\
             \import {\n\
             \   createJobFunctionSchema,\n\
             \   createJobFunctionsResponseSchema\n\
             \ } from './data/job-functions-classes';"
            ]
  let neg = []
  testParser parseTop pos neg

{-
test_ = do
  let pos = [""]
  let neg = [""]
  testParser _ pos neg
-}

main :: IO ()
main = htfMain htf_thisModulesTests
