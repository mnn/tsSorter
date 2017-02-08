module Lib where

import           Control.Applicative           ((<|>))
import           Control.Arrow                 ((>>>))
import           Control.Lens                  ((&), (<&>))
import           Control.Monad                 (void)
import           Data.Char                     (toLower)
import           Data.List                     (groupBy, intercalate,
                                                intersperse, nubBy, partition,
                                                sortBy)
import           Data.Maybe                    (fromMaybe)
import           Data.Ord                      (Ordering)
import           Data.String.Utils             (startswith)
import           Text.Parsec.Char
import           Text.ParserCombinators.Parsec hiding (spaces, (<|>))
import           Text.Regex                    (mkRegex, subRegex)

newtype Code = Code [CodeGroup] deriving (Eq, Show)

newtype CodeGroup = CodeGroup [CodePart] deriving (Eq, Show)

data CodePart = CodePartImport Import
              | CodePartRaw String
              deriving (Eq, Show)

newtype ImportSourceFile = ImportSourceFile String deriving (Eq, Show, Ord)

data ImportBeforeFromPart = ImportBeforeFromPartSimple ImportPart
                          | ImportBeforeFromPartGroup [ImportPart]
                          deriving (Eq, Show)

data Import = Import ImportBeforeFromPart ImportSourceFile deriving (Eq, Show)

-- import * as validator -> ImportPart "validator" (Just "*")
-- import validator -> ImportPart "validator" Nothing
data ImportPart = ImportPart String (Maybe String) deriving (Eq, Show)

data DataProcessingResult = DataProcessingResult String String

anyNewLine :: Parser ()
anyNewLine = void $ newline <|> crlf

parseImportGroupAlias :: Parser String
parseImportGroupAlias = many1 $ alphaNum <|> oneOf "_$"

parseImportSourceObjectName :: Parser String
parseImportSourceObjectName = many1 alphaNum

parseImportSourceObjectNameWithStar :: Parser String
parseImportSourceObjectNameWithStar = string "*" <|> parseImportSourceObjectName

parseAliasPart :: Parser String
parseAliasPart = do
  string "as"
  spaces
  parseImportGroupAlias

parseAliasPartPrefixedWithSpaces :: Parser String
parseAliasPartPrefixedWithSpaces = spaces >> parseAliasPart

parseImportPart :: Bool -> Parser ImportPart
parseImportPart allowStar = do
  name <- try parseName
  aliasOpt <- optionMaybe $ try parseAliasPartPrefixedWithSpaces
  return $ ImportPart name aliasOpt where
  parseName = if allowStar then parseImportSourceObjectNameWithStar else parseImportSourceObjectName

parseSimpleImport :: Parser ImportBeforeFromPart
parseSimpleImport = do
  part <- parseImportPart True
  return $ ImportBeforeFromPartSimple part

parseGroupImportDelim :: Parser ()
parseGroupImportDelim = do
  spaces
  char ','
  spaces

parseRawGroupImportParts :: Parser [ImportPart]
parseRawGroupImportParts = sepBy1 (try $ parseImportPart False) (try parseGroupImportDelim)

parseGroupImport :: Parser ImportBeforeFromPart
parseGroupImport = do
--  parts <- between (char '{' >> spaces) (spaces >> char '}') parseRawGroupImportParts
  char '{'
  spaces
  parts <- parseRawGroupImportParts
  spaces
  char '}'
  return $ ImportBeforeFromPartGroup parts

-- just a simple names, no special handling of escapes
parseFileName :: Parser String
parseFileName = many1 (alphaNum <|> oneOf "./@-")

parseFileNameString :: Parser String
parseFileNameString = do
  f <- char '\'' <|> char '"'
  name <- parseFileName
  char f
  return name

parseImport :: Parser Import
parseImport = do
  string "import"
  spaces
  beforeFromPart <- parseSimpleImport <|> parseGroupImport
  spaces
  string "from"
  spaces
  name <- parseFileNameString
  char ';'
  return $ Import beforeFromPart (ImportSourceFile name)

parseCodePartImport :: Parser CodePart
parseCodePartImport = parseImport >>= \x -> return $ CodePartImport x

parseCodePart :: Parser CodePart
parseCodePart = parseCodePartImport

parseCodeGroup :: Parser CodeGroup
parseCodeGroup = do
  parts <- lazySepBy1 parseCodePart anyNewLine
  return $ CodeGroup parts

lazySepBy1 :: Parser p -> Parser sep -> Parser [p]
lazySepBy1 parser separator = do
  x <- parser
  xs <- many (try $ separator >> parser)
  return (x:xs)

dropLeadingNewLine :: String -> String
dropLeadingNewLine xs = case xs of
  '\n':ys -> ys
  ys      -> ys

applyN n f x
  | n < 1 = x
  | otherwise = applyN (n-1) f (f x)

parseTop :: Parser Code
parseTop = do
  parts <- option [] $ lazySepBy1 parseCodeGroup (anyNewLine >> many1 anyNewLine)
  restRaw <- many anyChar
  let rest = applyN 2 dropLeadingNewLine restRaw -- getting rid of redundant new lines
  eof
  return $ Code $ parts ++ [CodeGroup [CodePartRaw rest]]

removeRecurringNewLines :: String -> String
removeRecurringNewLines x = subRegex (mkRegex "^\n{1,}$") x ""

removeTailingNewLine :: String -> String
removeTailingNewLine = reverse . dropLeadingNewLine . reverse

dropTailingEmptyLine :: String -> String
dropTailingEmptyLine x = let ls = lines x
                             lst = last ls
                           in if lst == "" then ls & init & unlines
                                           else x

class TsRender a where
  renderTsCode :: a -> String

instance TsRender Code where
  renderTsCode (Code xs) = map renderTsCode xs & intercalate "\n\n" & removeRecurringNewLines
    -- & applyN 2 dropTailingEmptyLine

instance TsRender CodeGroup where
  renderTsCode (CodeGroup xs) = intercalate "\n" $ map renderTsCode xs

instance TsRender CodePart where
  renderTsCode x = case x of
    (CodePartRaw code) -> code
    (CodePartImport i) -> renderTsCode i

instance TsRender Import where
  renderTsCode (Import bef src) = "import " ++ renderTsCode bef ++ " from " ++ renderTsCode src ++ ";"

instance TsRender ImportSourceFile where
  renderTsCode (ImportSourceFile x) = "'" ++ x ++ "'"

instance TsRender ImportBeforeFromPart where
  renderTsCode (ImportBeforeFromPartSimple x) = renderTsCode x
  renderTsCode (ImportBeforeFromPartGroup xs) = "{" ++ r ++ "}" where
    r = if len <= maxLenToFitLine
      then " " ++ intercalate ", " (map renderTsCode xs) ++ " "
      else "\n" ++ intercalate ",\n" (map (renderTsCode >>> ("  "++)) xs) ++ "\n"
    len = xs & map (renderTsCode >>> length) & intersperse 2 & sum
    maxLenToFitLine = 77

instance TsRender ImportPart where
  renderTsCode (ImportPart name aliasOpt) = name ++ aliasPart where
    aliasPart = fromMaybe "" $ aliasOpt <&> (\a->" as " ++ a)

class SortImports a where
  sortImports :: a -> a

instance SortImports Code where
  sortImports (Code xs) = Code $ map sortImports xs

instance SortImports CodeGroup where
  sortImports (CodeGroup xs) = sortedByFile & sortNamedImports & CodeGroup where
    sortedByFile :: [CodePart]
    sortedByFile = xs & sortBy cmp & dotsToEnd & atSignToTop
    cmp (CodePartImport a) (CodePartImport b) = cmpImp a b
    cmp _ _                                   = EQ
    cmpImp (Import aBef (ImportSourceFile aSrc)) (Import bBef (ImportSourceFile bSrc)) = compareStringIgnoringCase aSrc bSrc
    sortNamedImports = map sortNamedImportOfOne
    sortNamedImportOfOne :: CodePart -> CodePart
    sortNamedImportOfOne x = case x of
      (CodePartImport (Import bef src)) -> CodePartImport (Import (sortBefPart bef) src)
      (CodePartRaw _) -> x
    sortBefPart x = case x of
      (ImportBeforeFromPartSimple _) -> x
      (ImportBeforeFromPartGroup xs) -> ImportBeforeFromPartGroup $ sortBy cmpImportPart xs
    cmpImportPart (ImportPart aName aAlias) (ImportPart bName bAlias) = compareStringIgnoringCase aName bName
    overFileNamePair f (CodePartImport (Import _ (ImportSourceFile a))) (CodePartImport (Import _ (ImportSourceFile b))) = f a b
    overFileName _ f (CodePartImport (Import _ (ImportSourceFile a))) = f a
    overFileName x _ _                                                = x
    dotsToEnd xs = let (dots, rest) = partition (overFileName False $ startswith ".") xs
                     in rest ++ dots
    atSignToTop xs = let (ats, rest) = partition (overFileName False $ startswith "@") xs
                     in ats ++ rest

strToLower :: String -> String
strToLower = map toLower

compareStringIgnoringCase :: String -> String -> Ordering
compareStringIgnoringCase a b = compare (strToLower a) (strToLower b)

unifyImportsInGroups :: Code -> Code
unifyImportsInGroups (Code groups) = Code resGroups where
  resGroups = map processGroup groups
  processGroup (CodeGroup parts) = CodeGroup (processParts parts)
  processParts :: [CodePart] -> [CodePart]
  processParts parts = parts & groupBySourceFile & mergeGroups & mix parts &removeDuplicatedFromFirst & assignToFirst where
    groupBySourceFile :: [CodePart] -> [[CodePart]]
    groupBySourceFile = groupBy groupFn
    groupFn a b = getSrcFromPart a == getSrcFromPart b && isNotImportBeforePartSimple a && isNotImportBeforePartSimple b
    getSrcFromPart (CodePartImport (Import _ (ImportSourceFile x))) = x
    getSrcFromPart _                                                = ""
    isImportBeforePartSimple (CodePartImport (Import (ImportBeforeFromPartSimple _) _)) = True
    isImportBeforePartSimple _ = False
    isNotImportBeforePartSimple = not . isImportBeforePartSimple
    mergeGroups :: [[CodePart]] -> [CodePart]
    mergeGroups = map mergeGroup
    mergeGroup :: [CodePart] -> CodePart
    mergeGroup = foldl1 mergeGroupFoldFn
    mergeGroupFoldFn (CodePartImport (Import (ImportBeforeFromPartGroup accBef) accSrc)) (CodePartImport (Import (ImportBeforeFromPartGroup xBef) _)) =
      CodePartImport (Import (ImportBeforeFromPartGroup $ accBef ++ xBef) accSrc)
    mergeGroupFoldFn a b = error $ "mergeGroupFoldFn crash:\n" ++ show a ++ "\n" ++ show b
    mix :: [CodePart] -> [CodePart] -> ([CodePart], [CodePart])
    mix = (,)
    removeDuplicatedFromFirst :: ([CodePart], [CodePart]) -> ([CodePart], [CodePart])
    removeDuplicatedFromFirst (x, y) = let r = nubBy groupFn x in (r, y)
    assignToFirst :: ([CodePart], [CodePart]) -> [CodePart]
    assignToFirst (tpl, merged) = let db = map (\x->((getSrcFromPart x, isImportBeforePartSimple x), x)) merged
                                      m partTpl = fromMaybe (error "transformation or map is broken")
                                        (lookup (getSrcFromPart partTpl, isImportBeforePartSimple partTpl) db)
                                  in map m tpl

formatCode :: Code -> String
formatCode (Code groups) = formatGroups groups & map ("CodeGroup\n"++) & intercalate "\n\n" where
  formatGroups :: [CodeGroup] -> [String]
  formatGroups groups = groups & map formatGroup
  formatGroup :: CodeGroup -> String
  formatGroup (CodeGroup parts) = parts & map formatCodePart & intercalate "\n"
  formatCodePart :: CodePart -> String
  formatCodePart x = "  " ++ show x

preprocess :: Code -> Code
preprocess = unifyImportsInGroups >>> sortImports

processData :: String -> Either String DataProcessingResult
processData input = output where
  parsed = parse parseTop "" input
  output = case parsed of
    Left err -> Left $ "Error in parsing: " ++ show err
    Right x  -> let preprocessed = x & preprocess
                    res          = preprocessed & renderTsCode
                    log          = "-> parsed:\n" ++ formatCode x ++ "\n\n-> preprocessed:\n" ++ formatCode preprocessed
                      in Right (DataProcessingResult res log)
