
-- The builtin values and primitives, which any execution engine must support.
module Builtin(BuiltinError,BV(..),Prim1(..),Prim2(..),apply1,apply2) where

-- The builtins are stratified by arity.
data BV = Num Int | Str String | Bool Bool

data Prim1 = ShowInt
  deriving (Eq,Ord,Show)

data Prim2 = Add | Sub | Mul | ModInt | EqInt | LessInt | StringAppend
  deriving (Eq,Ord,Show)

instance Show BV where
  show = \case
    Num n -> show n
    Str s -> show s
    Bool b -> show b

data BuiltinError = BuiltinError { unBuiltinError :: String }
instance Show BuiltinError where show = unBuiltinError

apply1 :: Prim1 -> BV -> Either BuiltinError BV
apply1 prim = case prim of
  ShowInt -> \case
    Num n1 -> Right (Str (show n1))
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

  EqInt -> \case
    (Num n1, Num n2) -> Right (Bool (n1 == n2))
    vv -> Left $ BuiltinError $ "type error: " <> show (prim,vv)

  LessInt -> \case
    (Num n1, Num n2) -> Right (Bool (n1 < n2))
    vv -> Left $ BuiltinError $ "type error: " <> show (prim,vv)

  StringAppend -> \case
    (Str s1, Str s2) -> Right (Str (s1 ++ s2))
    vv -> Left $ BuiltinError $ "type error: " <> show (prim,vv)
