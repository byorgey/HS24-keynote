{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cats where

import Control.Monad (when)
import Control.Monad.Combinators.Expr
import Data.Bifunctor (first)
import Data.Functor (void)
import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

data Ty where
  N :: Ty
  B :: Ty
  T :: Ty
  (:->:) :: Ty -> Ty -> Ty
  deriving (Eq, Show)

infixr 0 :->:

type Var = Text

data Const = CCat | CShow | CSub | CEQ | CGT | CNot
  deriving (Eq, Show)

data Term where
  TNat :: Integer -> Term
  TTxt :: Text -> Term
  TConst :: Const -> Term
  TVar :: Var -> Term
  TLetrec :: Var -> Ty -> Term -> Term -> Term
  TLam :: Var -> Ty -> Term -> Term
  TApp :: Term -> Term -> Term
  TCond :: [(Term, Term)] -> Term
  deriving (Show)

------------------------------------------------------------
-- Parser
------------------------------------------------------------

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens, braces :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")

natural :: Parser Integer
natural = lexeme L.decimal <?> "natural number"

lambda :: Parser Text
lambda = symbol "\\" <|> symbol "λ"

identifier :: Parser Text
identifier = lexeme (T.cons <$> letterChar <*> (T.pack <$> many alphaNumChar))

reserved :: Text -> Parser ()
reserved = void . lexeme . string

opChar :: String
opChar = "~!@#$%^&*-+=/\\<>"

operator :: Text -> Parser ()
operator t = (lexeme . try) (string t *> notFollowedBy (oneOf opChar))

parseTerm :: Parser Term
parseTerm = makeExprParser parseAtom table <?> "expression"
 where
  mkBin c = TApp . TApp (TConst c)
  table =
    [ [InfixL (mkBin CCat <$ string "")]
    , [InfixL (TApp <$ symbol "@")]
    , [InfixL (mkBin CSub <$ operator "-")]
    ,
      [ InfixN (mkBin CEQ <$ operator "=")
      , InfixN (mkBin CGT <$ operator ">")
      ]
    , [Prefix (TApp (TConst CShow) <$ operator "%")]
    , [Prefix (TApp (TConst CNot) <$ operator "¬")]
    ]

parseAtom :: Parser Term
parseAtom =
  TNat <$> natural
    <|> TTxt <$> lexeme (T.pack <$> (char '"' *> manyTill L.charLiteral (char '"')))
    <|> TLam <$> (lambda *> identifier) <*> (symbol ":" *> parseType) <*> (symbol "." *> parseTerm)
    <|> TVar <$> identifier
    <|> parseLet
    <|> parseCond
    <|> parens parseTerm

parseLet :: Parser Term
parseLet = do
  TLetrec
    <$> (symbol "[" *> identifier)
    <*> (symbol ":" *> parseType)
    <*> (symbol ":=" *> parseTerm)
    <*> (symbol "]" *> parseTerm)

parseCond :: Parser Term
parseCond = braces $ TCond <$> ((,) <$> parseTerm <*> (symbol "=>" *> parseTerm)) `sepBy` symbol "|"

parseType :: Parser Ty
parseType = makeExprParser parseTypeAtom [[InfixR $ (:->:) <$ (symbol "->" <|> symbol "→")]]

parseTypeAtom :: Parser Ty
parseTypeAtom =
  N <$ reserved "N"
    <|> T <$ reserved "T"
    <|> B <$ reserved "B"
    <|> parens parseType

readTerm :: Text -> Either Text Term
readTerm t = first (T.pack . errorBundlePretty) (runParser parseTerm "" t)

------------------------------------------------------------
-- Typechecker
------------------------------------------------------------

type Ctx = Map Var Ty

data TypeError
  = Unbound !Var
  | Mismatch !Ty !Ty
  | NotFun !Ty
  deriving (Show)

showT :: Show a => a -> Text
showT = T.pack . show

prettyTypeError :: TypeError -> Text
prettyTypeError (Unbound x) = "Undefined variable " <> x
prettyTypeError (Mismatch ty1 ty2) = "Type mismatch: expected " <> showT ty1 <> ", got " <> showT ty2
prettyTypeError (NotFun ty) = "Expected " <> showT ty <> " to be a function type, but it is not"

check :: Ctx -> Ty -> Term -> Either TypeError ()
check ctx ty t = do
  ty' <- infer ctx t
  when (ty /= ty') $ Left (Mismatch ty ty')

infer :: Ctx -> Term -> Either TypeError Ty
infer ctx = \case
  TNat _ -> pure N
  TTxt _ -> pure T
  TConst c -> pure $ inferConst c
  TVar x -> maybe (Left (Unbound x)) Right $ M.lookup x ctx
  TLetrec x xTy t1 t2 -> do
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
      _notFun -> Left $ NotFun ty1
  TCond cs -> do
    mapM_ (\(c, b) -> check ctx B c *> check ctx T b) cs
    pure T

inferConst :: Const -> Ty
inferConst = \case
  CCat -> T :->: T :->: T
  CSub -> N :->: N :->: N
  CEQ -> N :->: N :->: B
  CGT -> N :->: N :->: B
  CNot -> B :->: B
  CShow -> N :->: T

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
  TLetrec x _ t1 t2 ->
    let (e', v1) = (M.insert x v1 e, interp e' t1)
     in interp e' t2
  TLam x _ t -> VClo e x t
  TApp t1 t2 ->
    let v1 = interp e t1
        v2 = interp e t2
     in case v1 of
          VFun f -> f v2
          VClo e' x t -> interp (M.insert x v2 e') t
          _other -> error "Got non-function value in TApp"
  TCond cs -> interpCond e cs

interpCond :: Env -> [(Term, Term)] -> Value
interpCond e = \case
  [] -> VTxt ""
  ((c, b) : cs) -> case interp e c of
    VBool True -> interp e b
    VBool False -> interpCond e cs
    _other -> error "Got non-bool value in condition"

withTxt :: (Text -> Value) -> Value
withTxt f = VFun $ \case
  VTxt x -> f x
  _other -> error "Unexpected non-text value"

withNat :: (Integer -> Value) -> Value
withNat f = VFun $ \case
  VNat x -> f x
  _other -> error "Unexpected non-natural value"

withBool :: (Bool -> Value) -> Value
withBool f = VFun $ \case
  VBool b -> f b
  _other -> error "Unexpected non-bool value"

interpConst :: Const -> Value
interpConst = \case
  CCat -> withTxt $ \x -> withTxt $ \y -> VTxt (x <> y)
  CSub -> withNat $ \x -> withNat $ \y -> VNat (max 0 (x - y))
  CEQ -> withNat $ \x -> withNat $ \y -> VBool (x == y)
  CGT -> withNat $ \x -> withNat $ \y -> VBool (x > y)
  CNot -> withBool $ \x -> VBool (not x)
  CShow -> withNat $ \n -> VTxt (T.pack (show n))
