{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Catena where

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
  TyN :: Ty
  TyB :: Ty
  TyT :: Ty
  (:->:) :: Ty -> Ty -> Ty
  deriving (Eq, Show)

infixr 0 :->:

type Var = Text

data Const = CCat | CAdd | CSub | CMul | CDiv | CEQ | CGT | CLT | CGE | CLE | CNE | CNot | CAnd | COr | CShow
  deriving (Eq, Show)

data Term where
  TNat :: Integer -> Term
  TTxt :: Text -> Term
  TConst :: Const -> Term
  TVar :: Var -> Term
  TLet :: Var -> Ty -> Term -> Term -> Term
  TLam :: Var -> Ty -> Term -> Term
  TApp :: Term -> Term -> Term
  TCond :: [(Term, Term)] -> Term
  deriving (Show)

------------------------------------------------------------
-- Parser
------------------------------------------------------------

type Parser = Parsec Void Text

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
    ,
      [ InfixL (mkBin CMul <$ operator "*")
      , InfixL (mkBin CDiv <$ operator "/")
      ]
    ,
      [ InfixL (mkBin CAdd <$ operator "+")
      , InfixL (mkBin CSub <$ operator "-")
      ]
    ,
      [ InfixN (mkBin CEQ <$ operator "=")
      , InfixN (mkBin CLE <$ operator "≤")
      , InfixN (mkBin CGE <$ operator "≥")
      , InfixN (mkBin CLT <$ operator "<")
      , InfixN (mkBin CGT <$ operator ">")
      , InfixN (mkBin CNE <$ operator "≠")
      ]
    , [Prefix (TApp (TConst CShow) <$ operator "%")]
    , [Prefix (TApp (TConst CNot) <$ operator "¬")]
    , [InfixR (mkBin CAnd <$ operator "∧")]
    , [InfixR (mkBin COr <$ operator "∨")]
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
  let' <- brackets $ TLet <$> identifier <*> (symbol ":" *> parseType) <*> (symbol ":=" *> parseTerm)
  let' <$> parseTerm

parseCond :: Parser Term
parseCond = braces $ TCond <$> ((,) <$> parseTerm <*> (symbol "=>" *> parseTerm)) `sepBy` symbol "|"

parseType :: Parser Ty
parseType = makeExprParser parseTypeAtom table <?> "type"
 where
  table =
    [[InfixR ((:->:) <$ (symbol "->" <|> symbol "→"))]]

parseTypeAtom :: Parser Ty
parseTypeAtom =
  TyN <$ reserved "N"
    <|> TyT <$ reserved "T"
    <|> TyB <$ reserved "B"
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
      _notFun -> Left $ NotFun ty1
  TCond cs -> do
    mapM_ (\(c, b) -> check ctx TyB c *> check ctx TyT b) cs
    pure TyT

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
  CNE -> TyN :->: TyN :->: TyB
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
  TCond cs -> interpCond e cs

interpCond :: Env -> [(Term, Term)] -> Value
interpCond e = \case
  [] -> VTxt ""
  ((c, b) : cs) -> case interp e c of
    VBool True -> interp e b
    VBool False -> interpCond e cs

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
  CNE -> VFun $ \(VNat x) -> VFun $ \(VNat y) -> VBool (x /= y)
  CNot -> VFun $ \(VBool x) -> VBool (not x)
  CAnd -> VFun $ \(VBool x) -> VFun $ \(VBool y) -> VBool (x && y)
  COr -> VFun $ \(VBool x) -> VFun $ \(VBool y) -> VBool (x || y)
  CShow -> VFun $ \(VNat n) -> VTxt (T.pack (show n))
