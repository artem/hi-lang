{-# LANGUAGE OverloadedStrings #-}

module HW3.Parser where
import Data.Text (Text, pack)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import HW3.Base (HiExpr (..), HiValue (..), HiFun (..), HiAction (..))
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Word (Word8)
import Data.ByteString (pack, ByteString)
import qualified Data.Char as Char
import Data.Functor (void)
import Data.Char (isAlpha, isAlphaNum)
import Data.List (intercalate)

type Parser = Parsec Void String

-- | Helper functions
lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: String -> Parser String
symbol = L.symbol space

-- | Parse functional keywords
pFun :: Parser HiFun
pFun = choice
  [ HiFunDiv <$ symbol "div"
  , HiFunMul <$ symbol "mul"
  , HiFunAdd <$ symbol "add"
  , HiFunSub <$ symbol "sub"
  , HiFunIf              <$ symbol "if"
  , HiFunNotEquals       <$ symbol "not-equals"
  , HiFunNotGreaterThan  <$ symbol "not-greater-than"
  , HiFunNotLessThan     <$ symbol "not-less-than"
  , HiFunEquals          <$ symbol "equals"
  , HiFunGreaterThan     <$ symbol "greater-than"
  , HiFunLessThan        <$ symbol "less-than"
  , HiFunOr              <$ symbol "or"
  , HiFunAnd             <$ symbol "and"
  , HiFunNot             <$ symbol "not"
  , HiFunLength  <$ symbol "length"
  , HiFunToUpper <$ symbol "to-upper"
  , HiFunToLower <$ symbol "to-lower"
  , HiFunReverse <$ symbol "reverse"
  , HiFunTrim    <$ symbol "trim"
  , HiFunList  <$ symbol "list"
  , HiFunRange <$ symbol "range"
  , HiFunFold  <$ symbol "fold"
  , HiFunPackBytes   <$ symbol "pack-bytes"
  , HiFunUnpackBytes <$ symbol "unpack-bytes"
  , HiFunEncodeUtf8  <$ symbol "encode-utf8"
  , HiFunDecodeUtf8  <$ symbol "decode-utf8"
  , HiFunZip         <$ symbol "zip"
  , HiFunUnzip       <$ symbol "unzip"
  , HiFunSerialise   <$ symbol "serialise"
  , HiFunDeserialise <$ symbol "deserialise"
  , HiFunRead  <$ symbol "read"
  , HiFunWrite <$ symbol "write"
  , HiFunMkDir <$ symbol "mkdir"
  , HiFunChDir <$ symbol "cd"
  , HiFunParseTime <$ symbol "parse-time"
  , HiFunRand <$ symbol "rand"
  , HiFunEcho <$ symbol "echo"
  , HiFunCount  <$ symbol "count"
  , HiFunKeys   <$ symbol "keys"
  , HiFunValues <$ symbol "values"
  , HiFunInvert <$ symbol "invert" ]

-- | Operator table for makeExprParser
consBinOp :: HiFun -> HiExpr -> HiExpr -> HiExpr
consBinOp c a b = HiExprApply (HiExprValue $ HiValueFunction c) [a, b]

noEq :: String -> Parser String
noEq x = lexeme $ try $ string x <* notFollowedBy (char '=')

operators :: [[Operator Parser HiExpr]]
operators =
  [ [ InfixL (consBinOp HiFunDiv <$ noEq "/")
    , InfixL (consBinOp HiFunMul <$ symbol "*") ]
  , [ InfixL (consBinOp HiFunAdd <$ symbol "+")
    , InfixL (consBinOp HiFunSub <$ symbol "-") ]
  , [ InfixN (consBinOp HiFunLessThan       <$ noEq "<")
    , InfixN (consBinOp HiFunGreaterThan    <$ noEq ">")
    , InfixN (consBinOp HiFunNotLessThan    <$ symbol ">=")
    , InfixN (consBinOp HiFunNotGreaterThan <$ symbol "<=")
    , InfixN (consBinOp HiFunEquals         <$ symbol "==")
    , InfixN (consBinOp HiFunNotEquals      <$ symbol "/=") ]
  , [ InfixR (consBinOp HiFunAnd <$ symbol "&&") ]
  , [ InfixR (consBinOp HiFunOr  <$ symbol "||") ]
  ]

-- | Infix operators
pOp :: Parser HiExpr
pOp = makeExprParser pExpr operators

-- | Parse boolean
pBool :: Parser Bool
pBool = choice
  [ True <$ symbol "true"
  , False <$ symbol "false" ]

-- | Comma-separated expressions
pParamList :: Parser [HiExpr]
pParamList = try $ lexeme $ do
    ff <- pOp
    tl <- try $ (symbol "," >> pParamList)
      <|> return []
    return $ ff : tl
  <|> return []

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parse bang, dot and argument application
pPostfix :: HiExpr -> Parser HiExpr
pPostfix cur = try (pAppl cur) <|> pExec cur <|> pProp cur <|> return cur

pProp :: HiExpr -> Parser HiExpr
pProp fun = do
    void (symbol ".")
    key <- lexeme $ ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'
    let arg = HiExprValue $ HiValueString $ Data.Text.pack $ intercalate "-" key
    pPostfix (HiExprApply fun [arg])

pAppl :: HiExpr -> Parser HiExpr
pAppl fun = do
    args <- parens pParamList
    pPostfix (HiExprApply fun args)

pExec :: HiExpr -> Parser HiExpr
pExec fun = do
    void (symbol "!")
    pPostfix (HiExprRun fun)

-- | Parse expression (no infix)
pExpr :: Parser HiExpr
pExpr = (parens pOp <|> try pList <|> try pDict <|> HiExprValue <$> pValue) >>= pPostfix

pValue :: Parser HiValue
pValue = try (HiValueFunction <$> pFun)
  <|> (HiValueBool <$> pBool)
  <|> (HiValueNull <$ symbol "null")
  <|> (HiValueAction HiActionCwd <$ symbol "cwd")
  <|> (HiValueAction HiActionNow <$ symbol "now")
  <|> (HiValueNumber <$> pNum)
  <|> (HiValueString <$> pStr)
  <|> (HiValueBytes <$> pBs)

pDict :: Parser HiExpr
pDict = HiExprDict <$> between (symbol "{") (symbol "}") pMapPairs

pMapPairs :: Parser [(HiExpr, HiExpr)]
pMapPairs = try $ lexeme $ do
    ff <- pOp
    void (symbol ":")
    kk <- pOp
    tl <- try (symbol "," >> pMapPairs) <|> return []
    return $ (ff, kk) : tl
  <|> return []

-- | String literal
pStr :: Parser Text
pStr = Data.Text.pack <$> (char '"' *> manyTill L.charLiteral (symbol "\""))

-- | Parses into `list ( PARAMS )`
pList :: Parser HiExpr
pList = HiExprApply (HiExprValue (HiValueFunction HiFunList))
  <$> between (symbol "[") (symbol "]") pParamList

-- | ByteString [# #]
pBs :: Parser ByteString
pBs = Data.ByteString.pack <$> between (symbol "[#") (symbol "#]") pBytes

pBytes :: Parser [Word8]
pBytes = try $ lexeme $ do
    h <- hexDigitChar
    l <- hexDigitChar
    tl <- (space1 >> pBytes) <|> return []
    return $ b h l : tl
  <|> return []
  where
    b h l = fromIntegral (Char.digitToInt h * 16 + Char.digitToInt l)

pNum :: Parser Rational
pNum = lexeme $ toRational <$> L.signed space L.scientific

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (between space eof pOp) ""
