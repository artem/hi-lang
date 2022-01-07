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

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

pFun :: Parser HiFun
pFun = lexeme $ choice
  [ HiFunDiv <$ string "div"
  , HiFunMul <$ string "mul"
  , HiFunAdd <$ string "add"
  , HiFunSub <$ string "sub" ]

pParamList :: Parser [HiExpr]
pParamList = try $ lexeme $ do
    ff <- pExpr
    tl <- try $ do
        _ <- lexeme $ char ','
        pParamList
      <|> return []
    return $ ff : tl
  <|> return []


pAppl :: HiExpr -> Parser HiExpr
pAppl fun = do
    _ <- lexeme $ char '('
    args <- pParamList
    _ <- lexeme $ char ')'
    let cur = HiExprApply fun args
    try (pAppl cur) <|> return cur

pExpr :: Parser HiExpr
pExpr = do
    fun <- HiExprValue <$> pValue
    try $ pAppl fun <|> return fun

pValue :: Parser HiValue
pValue = (HiValueFunction <$> pFun) <|> (HiValueNumber <$> pNum)

pNum :: Parser Rational
pNum = lexeme $ toRational <$> L.signed space L.scientific

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (space >> pExpr <* eof) ""
