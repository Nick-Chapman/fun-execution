
module Predefined(defs) where

import Rep_Ast(Var(..),Exp(..),Def(..),mkELam)
import Builtin

defs :: [Def]
defs = mkDefs

  [ ("+", binop Builtin.Add)
  , ("-", binop Builtin.Sub)
  , ("*", binop Builtin.Mul)
  , ("%", binop Builtin.ModInt)
  , ("==", binop Builtin.EqInt)
  , ("<", binop Builtin.LessInt)
  , ("true", ECon $ Builtin.Bool True)
  , ("false", ECon $ Builtin.Bool False)
  , ("^", binop Builtin.StringAppend)
  , ("showInt", unop Builtin.ShowInt)
  , ("readInt", unop Builtin.ReadInt)
  , ("argv", unop Builtin.Argv)
  ]


mkDefs :: [(String,Exp)] -> [Def]
mkDefs = map $ \(x,rhs) -> Def (Var x) rhs

unop :: Builtin.Prim1 -> Exp
unop prim = mkELam x (EPrim1 prim (EVar x))
  where
    x = Var "x"

binop :: Builtin.Prim2 -> Exp
binop prim = mkELam x (mkELam y (EPrim2 prim (EVar x) (EVar y)))
  where
    x = Var "x"
    y = Var "y"
