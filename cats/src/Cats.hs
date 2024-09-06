{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cats where

import Control.Monad.Combinators.Expr
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

data Const = CCat
  deriving (Eq, Show)

data Term where
  TNat :: Integer -> Term
  TTxt :: Text -> Term
  TApp :: Term -> Term -> Term
  TConst :: Const -> Term
  deriving (Show, Eq)

------------------------------------------------------------ Parser

type Parser a = Parsec Void Text a

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

natural :: Parser Integer
natural = lexeme L.decimal

parseTerm :: Parser Term
parseTerm = makeExprParser parseAtom table
 where
  table =
    [ [InfixL (mkBin CCat <$ string "")]
    ]
  mkBin c a b = TApp (TApp (TConst c) a) b

parseAtom :: Parser Term
parseAtom =
  TNat <$> natural
    <|> TTxt . T.pack <$> (char '\"' *> manyTill L.charLiteral (symbol "\""))

------------------------------------------------------------ Interpreter

data Value = VTxt Text | VNat Integer | VFun (Value -> Value)
  deriving (Show)

instance Show (a -> b) where
  show _ = "!!!!!!"

interp :: Term -> Value
interp = \case
  TNat n -> VNat n
  TTxt t -> VTxt t
  TConst c -> interpConst c
  TApp t1 t2 -> case interp t1 of
    VFun f -> f (interp t2)
    _other -> error "Not a function!!"

withText :: (Text -> Value) -> (Value -> Value)
withText f = \case
  VTxt t -> f t
  _other -> error "Not a text!!!!"

interpConst :: Const -> Value
interpConst = \case
  CCat -> VFun $ withText $ \x -> VFun $ withText $ \y -> VTxt (T.append x y)
