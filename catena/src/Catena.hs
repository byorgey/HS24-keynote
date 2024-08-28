{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Catena where

import Control.Monad (guard, when)
import Control.Monad.Combinators.Expr
import Data.Functor (void)
import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

data Ty where
  TyN :: Ty
  TyB :: Ty
  TyT :: Ty
  (:->:) :: Ty -> Ty -> Ty
  deriving (Eq, Show)

infixr 0 :->:

type Var = Text

data Const = CCat | CAdd | CSub | CMul | CDiv | CEQ | CGT | CLT | CGE | CLE | CNot | CAnd | COr | CShow
  deriving (Eq, Show)

data Term where
  TNat :: Integer -> Term
  TTxt :: Text -> Term
  TConst :: Const -> Term
  TVar :: Var -> Term
  TLet :: Var -> Ty -> Term -> Term -> Term
  TLam :: Var -> Ty -> Term -> Term
  TApp :: Term -> Term -> Term
  TIf :: Term -> Term -> Term -> Term
  deriving (Show)

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

parens, brackets, braces :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
brackets = between (symbol "[") (symbol "]")
braces = between (symbol "{") (symbol "}")

natural :: Parser Integer
natural = lexeme L.decimal <?> "natural number"

lambda :: Parser Text
lambda = symbol "\\" <|> symbol "λ"

identifier :: Parser Text
identifier = lexeme (T.cons <$> letterChar <*> (T.pack <$> many alphaNumChar))

reserved :: Text -> Parser ()
reserved = void . lexeme . string

parseTerm :: Parser Term
parseTerm = makeExprParser parseAtom table <?> "expression"
 where
  mkBin c = TApp . TApp (TConst c)
  table =
    [ [InfixL (mkBin CCat <$ string "")]
    , [InfixL (TApp <$ symbol "@")]
    ,
      [ InfixL (mkBin CMul <$ symbol "*")
      , InfixL (mkBin CDiv <$ symbol "/")
      ]
    ,
      [ InfixL (mkBin CAdd <$ symbol "+")
      , InfixL (mkBin CSub <$ symbol "-")
      ]
    ,
      [ InfixN (mkBin CEQ <$ symbol "=")
      , InfixN (mkBin CLE <$ symbol "<=")
      , InfixN (mkBin CGE <$ symbol ">=")
      , InfixN (mkBin CLT <$ symbol "<")
      , InfixN (mkBin CGT <$ symbol ">")
      ]
    , [Prefix (TApp (TConst CShow) <$ symbol "%")]
    , [Prefix (TApp (TConst CNot) <$ symbol "~")]
    , [InfixR (mkBin CAnd <$ symbol "&")]
    , [InfixR (mkBin COr <$ symbol "|")]
    ]

parseAtom :: Parser Term
parseAtom =
  TNat <$> natural
    <|> TTxt <$> lexeme (T.pack <$> (char '"' *> manyTill L.charLiteral (char '"')))
    <|> TLam <$> (lambda *> identifier) <*> (symbol ":" *> parseType) <*> (symbol "." *> parseTerm)
    <|> TVar <$> identifier
    <|> parseLet
    <|> parseIf
    <|> parens parseTerm

parseLet :: Parser Term
parseLet = do
  let' <- brackets $ TLet <$> identifier <*> (symbol ":" *> parseType) <*> (symbol ":=" *> parseTerm)
  let' <$> parseTerm

parseIf :: Parser Term
parseIf = braces $ TIf <$> parseTerm <*> (symbol "?" *> parseTerm) <*> (symbol ":" *> parseTerm)

parseType :: Parser Ty
parseType = makeExprParser parseTypeAtom table <?> "type"
 where
  table =
    [[InfixR ((:->:) <$ symbol "->")]]

parseTypeAtom :: Parser Ty
parseTypeAtom =
  TyN <$ reserved "N"
    <|> TyT <$ reserved "T"
    <|> TyB <$ reserved "B"
    <|> parens parseType

------------------------------------------------------------
-- Typechecker
------------------------------------------------------------

type Ctx = Map Var Ty

data TypeError
  = Unbound Var
  | Mismatch Ty Ty
  | NotFun Ty
  deriving (Show)

check :: Ctx -> Ty -> Term -> Either TypeError ()
check ctx ty t = do
  ty' <- infer ctx t
  when (ty /= ty') $ Left (Mismatch ty ty')

infer :: Ctx -> Term -> Either TypeError Ty
infer ctx = \case
  TNat _ -> pure TyN
  TTxt _ -> pure TyT
  TConst c -> pure $ inferConst c
  TVar x -> maybe (Left (Unbound x)) Right $ M.lookup x ctx
  TLet x xTy t1 t2 -> do
    let ctx' = M.insert x xTy ctx
    check ctx' xTy t1
    infer ctx' t2
  TLam x xTy t -> (xTy :->:) <$> infer (M.insert x xTy ctx) t
  TApp t1 t2 -> do
    ty1 <- infer ctx t1
    case ty1 of
      tyA :->: tyB -> do
        check ctx tyA t2
        pure tyB
      _ -> Left $ NotFun ty1
  TIf c t1 t2 -> do
    check ctx TyB c
    tyA <- infer ctx t1
    check ctx tyA t2
    pure tyA

inferConst :: Const -> Ty
inferConst = \case
  CCat -> TyT :->: TyT :->: TyT
  CAdd -> TyN :->: TyN :->: TyN
  CSub -> TyN :->: TyN :->: TyN
  CMul -> TyN :->: TyN :->: TyN
  CDiv -> TyN :->: TyN :->: TyN
  CEQ -> TyN :->: TyN :->: TyB
  CGT -> TyN :->: TyN :->: TyB
  CLT -> TyN :->: TyN :->: TyB
  CGE -> TyN :->: TyN :->: TyB
  CLE -> TyN :->: TyN :->: TyB
  CNot -> TyB :->: TyB
  CAnd -> TyB :->: TyB :->: TyB
  COr -> TyB :->: TyB :->: TyB
  CShow -> TyN :->: TyT

------------------------------------------------------------
-- Interpreter
------------------------------------------------------------

data Value = VNat !Integer | VBool !Bool | VTxt !Text | VFun !(Value -> Value) | VClo !Env !Var !Term
type Env = Map Var Value

instance Show Value where
  show = \case
    VNat n -> show n
    VBool b -> show b
    VTxt t -> T.unpack t
    VFun {} -> "<fun>"
    VClo {} -> "<clo>"

interp :: Env -> Term -> Value
interp e = \case
  TNat n -> VNat n
  TTxt t -> VTxt t
  TConst c -> interpConst c
  TVar x -> e ! x
  TLet x _ t1 t2 ->
    let (e', v1) = (M.insert x v1 e, interp e' t1)
     in interp e' t2
  TLam x _ t -> VClo e x t
  TApp t1 t2 ->
    let v1 = interp e t1
        v2 = interp e t2
     in case v1 of
          VFun f -> f v2
          VClo e' x t -> interp (M.insert x v2 e') t
  TIf c t1 t2 -> case interp e c of
    VBool True -> interp e t1
    VBool False -> interp e t2

interpConst :: Const -> Value
interpConst = \case
  CCat -> VFun $ \(VTxt x) -> VFun $ \(VTxt y) -> VTxt (x <> y)
  CAdd -> VFun $ \(VNat x) -> VFun $ \(VNat y) -> VNat (x + y)
  CSub -> VFun $ \(VNat x) -> VFun $ \(VNat y) -> VNat (max 0 (x - y))
  CMul -> VFun $ \(VNat x) -> VFun $ \(VNat y) -> VNat (x * y)
  CDiv -> VFun $ \(VNat x) -> VFun $ \(VNat y) -> VNat (x `div` y)
  CEQ -> VFun $ \(VNat x) -> VFun $ \(VNat y) -> VBool (x == y)
  CGT -> VFun $ \(VNat x) -> VFun $ \(VNat y) -> VBool (x > y)
  CLT -> VFun $ \(VNat x) -> VFun $ \(VNat y) -> VBool (x < y)
  CGE -> VFun $ \(VNat x) -> VFun $ \(VNat y) -> VBool (x >= y)
  CLE -> VFun $ \(VNat x) -> VFun $ \(VNat y) -> VBool (x <= y)
  CNot -> VFun $ \(VBool x) -> VBool (not x)
  CAnd -> VFun $ \(VBool x) -> VFun $ \(VBool y) -> VBool (x && y)
  COr -> VFun $ \(VBool x) -> VFun $ \(VBool y) -> VBool (x || y)
  CShow -> VFun $ \(VNat n) -> VTxt (T.pack (show n))
