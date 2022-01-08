{-# LANGUAGE OverloadedStrings #-}

module HW3.Parser where
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import HW3.Base (HiExpr (..), HiValue (..), HiFun (..))
import qualified Data.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Functor (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: String -> Parser String
symbol = L.symbol space

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
  , HiFunNot             <$ symbol "not" ]

consBinOp :: HiFun -> HiExpr -> HiExpr -> HiExpr
consBinOp c a b = HiExprApply (HiExprValue $ HiValueFunction c) [a, b]

operators :: [[Operator Parser HiExpr]]
operators =
  [ [ InfixL (consBinOp HiFunDiv <$ (lexeme . try) (string "/" <* notFollowedBy (char '=')))
    , InfixL (consBinOp HiFunMul <$ symbol "*") ]
  , [ InfixL (consBinOp HiFunAdd <$ symbol "+")
    , InfixL (consBinOp HiFunSub <$ symbol "-") ]
  , [ InfixN (consBinOp HiFunLessThan       <$ symbol "<")
    , InfixN (consBinOp HiFunGreaterThan    <$ symbol ">")
    , InfixN (consBinOp HiFunNotLessThan    <$ symbol ">=")
    , InfixN (consBinOp HiFunNotGreaterThan <$ symbol "<=")
    , InfixN (consBinOp HiFunEquals         <$ symbol "==")
    , InfixN (consBinOp HiFunNotEquals      <$ symbol "/=") ]
  , [ InfixR (consBinOp HiFunAnd <$ symbol "&&") ]
  , [ InfixR (consBinOp HiFunOr  <$ symbol "||") ]
  ]

pOp :: Parser HiExpr
pOp = makeExprParser pExpr operators

pBool :: Parser Bool
pBool = choice
  [ True <$ symbol "true"
  , False <$ symbol "false" ]

pParamList :: Parser [HiExpr]
pParamList = try $ lexeme $ do
    ff <- pOp
    tl <- try $ do
        _ <- symbol ","
        pParamList
      <|> return []
    return $ ff : tl
  <|> return []

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pAppl :: HiExpr -> Parser HiExpr
pAppl fun = do
    args <- parens pParamList
    let cur = HiExprApply fun args
    try (pAppl cur) <|> return cur

pExpr :: Parser HiExpr
pExpr = do
    fun <- parens pOp <|> HiExprValue <$> pValue
    try $ pAppl fun <|> return fun

pValue :: Parser HiValue
pValue = try (HiValueFunction <$> pFun) <|> (HiValueBool <$> pBool) <|> (HiValueNumber <$> pNum)

pNum :: Parser Rational
pNum = lexeme $ toRational <$> L.signed space L.scientific

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (between space eof pOp) ""
