
-- The builtin values and primitives, which any execution engine must support.
module Builtin(BuiltinError,BV(..),Prim1,Prim2(..),apply1,apply2) where

-- The builtins are stratified by arity.
data BV = Num Int | Str String | Bool Bool
data Prim1
data Prim2 = Add | Sub | Mul | EqInt deriving (Eq,Ord,Show)

instance Show BV where
  show = \case
    Num n -> show n
    Str s -> show s
    Bool b -> show b

data BuiltinError = BuiltinError { unBuiltinError :: String }
instance Show BuiltinError where show = unBuiltinError

apply1 :: Prim1 -> BV -> Either BuiltinError BV
apply1 = undefined

apply2 :: Prim2 -> (BV,BV) -> Either BuiltinError BV
apply2 = \case

  Add -> \case
    (Num n1, Num n2) -> Right (Num (n1+n2))
    vv -> Left $ BuiltinError $ "cant add non-numbers: " <> show vv

  Sub -> \case
    (Num n1, Num n2) -> Right (Num (n1-n2))
    vv -> Left $ BuiltinError $ "cant subtract non-numbers: " <> show vv

  Mul -> \case
    (Num n1, Num n2) -> Right (Num (n1*n2))
    vv -> Left $ BuiltinError $ "cant multiply non-numbers: " <> show vv

  EqInt -> \case
    (Num n1, Num n2) -> Right (Bool (n1 == n2))
    vv -> Left $ BuiltinError $ "cant compare non-numbers: " <> show vv
