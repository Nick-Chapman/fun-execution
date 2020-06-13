
-- The builtin values and primitives, which any execution engine must support.
module Builtin(BuiltinError,BV(..),Prim1(..),Prim2(..),apply1,apply2,CommandLineArgs(..)) where

-- The builtins are stratified by arity.
data BV = Num Int | Str String | Bool Bool | Char Char
  deriving (Eq)

data Prim1 = ShowChar | ShowInt | ReadInt | Argv | StrSize
  deriving (Eq,Ord,Show)

data Prim2 = Add | Sub | Mul | ModInt | EqChar | EqInt | EqString | LessInt | StringAppend | StrIndex
  deriving (Eq,Ord,Show)

instance Show BV where
  show = \case
    Num n -> show n
    Str s -> show s
    Bool b -> show b
    Char x -> show x

data BuiltinError = BuiltinError { unBuiltinError :: String }
instance Show BuiltinError where show = unBuiltinError

data CommandLineArgs = CommandLineArgs { argv :: Int -> String }

apply1 :: CommandLineArgs -> Prim1 -> BV -> Either BuiltinError BV
apply1 CommandLineArgs{argv} prim = case prim of
  ShowChar -> \case
    Char c1 -> Right (Str [c1])
    v -> Left $ BuiltinError $ "type error: " <> show (prim,v)

  ShowInt -> \case
    Num n1 -> Right (Str (show n1))
    v -> Left $ BuiltinError $ "type error: " <> show (prim,v)

  ReadInt -> \case
    Str s1 -> Right (Num (read s1))
    v -> Left $ BuiltinError $ "type error: " <> show (prim,v)

  Argv -> \case
    Num n -> Right (Str (argv n))
    v -> Left $ BuiltinError $ "type error: " <> show (prim,v)

  StrSize -> \case
    Str s -> Right (Num (length s))
    v -> Left $ BuiltinError $ "type error: " <> show (prim,v)

apply2 :: Prim2 -> (BV,BV) -> Either BuiltinError BV
apply2 prim = case prim of

  Add -> \case
    (Num n1, Num n2) -> Right (Num (n1+n2))
    vv -> Left $ BuiltinError $ "type error: " <> show (prim,vv)

  Sub -> \case
    (Num n1, Num n2) -> Right (Num (n1-n2))
    vv -> Left $ BuiltinError $ "type error: " <> show (prim,vv)

  Mul -> \case
    (Num n1, Num n2) -> Right (Num (n1*n2))
    vv -> Left $ BuiltinError $ "type error: " <> show (prim,vv)

  ModInt -> \case
    (Num n1, Num n2) -> Right (Num (n1 `mod` n2))
    vv -> Left $ BuiltinError $ "type error: " <> show (prim,vv)

  EqChar -> \case
    (Char x1, Char x2) -> Right (Bool (x1 == x2))
    vv -> Left $ BuiltinError $ "type error: " <> show (prim,vv)

  EqInt -> \case
    (Num n1, Num n2) -> Right (Bool (n1 == n2))
    vv -> Left $ BuiltinError $ "type error: " <> show (prim,vv)

  EqString -> \case
    (Str s1, Str s2) -> Right (Bool (s1 == s2))
    vv -> Left $ BuiltinError $ "type error: " <> show (prim,vv)

  LessInt -> \case
    (Num n1, Num n2) -> Right (Bool (n1 < n2))
    vv -> Left $ BuiltinError $ "type error: " <> show (prim,vv)

  StringAppend -> \case
    (Str s1, Str s2) -> Right (Str (s1 ++ s2))
    vv -> Left $ BuiltinError $ "type error: " <> show (prim,vv)

  StrIndex -> \case
    (Str s, Num n) -> Right (Char (s !! n))
    vv -> Left $ BuiltinError $ "type error: " <> show (prim,vv)
