{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Catena where

import Data.Functor (void)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

data Ty where
  TyN :: Ty
  TyB :: Ty
  TyT :: Ty
  TyFun :: Ty -> Ty -> Ty
  deriving (Eq, Show)

type Var = Text

data Const = CCat | CApp | CPlus | CMinus | CTimes | CDiv | CEQ | CGT | CLT | CGE | CLE | CNot | CCond
  deriving (Eq, Show)

data Term where
  TNat :: Integer -> Term
  TTxt :: Text -> Term
  TConst :: Const -> Term
  TLet :: Var -> Ty -> Term -> Term -> Term
  TLam :: Var -> Ty -> Term -> Term
  TApp :: Term -> Term -> Term
  deriving Show

------------------------------------------------------------
-- Parser
------------------------------------------------------------

type Parser = Parsec () Text

sc :: Parser ()
sc = L.space space1 lineComment empty
  where
   lineComment = L.skipLineComment "#"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

natural :: Parser Integer
natural = lexeme L.decimal <?> "natural number"

lambda :: Parser Text
lambda = symbol "\\" <|> symbol "λ"

-- No reserved keywords, only operators!

identifier :: Parser Text
identifier = lexeme (T.cons <$> letterChar <*> (T.pack <$> many alphaNumChar))

reserved :: Text -> Parser ()
reserved = void . lexeme . string

parseTerm :: Parser Term
parseTerm = makeExprParser parseAtom table <?> "expression"
 where
  mkBin c e1 e2 = TApp (TApp (TConst c) e1) e2
  table =
    [ [InfixL (mkBin CCat <$ string "")]
    ]

parseAtom :: Parser Term
parseAtom =
      TNat <$> natural
  <|> TTxt <$> lexeme (T.pack <$> (char '"' *> manyTill L.charLiteral (char '"')))
  <|> parseLet
  <|> parens parseTerm

parseLet :: Parser Term
parseLet = do
  let' <- brackets $ TLet <$> identifier <*> (symbol ":" *> parseType) <*> (symbol ":=" *> parseTerm)
  let' <$> parseTerm

parseType :: Parser Ty
parseType = makeExprParser parseTypeAtom table <?> "type"
 where
  table =
    [ [InfixR (TyFun <$ symbol "->")] ]

parseTypeAtom :: Parser Ty
parseTypeAtom =
  TyN <$ reserved "N" <|> TyT <$ reserved "T" <|> TyB <$ reserved "B"