
module Predefined(defs) where

import Rep1_Ast (Var(..),Exp(..),Def(..),mkELam)
import qualified Builtin

defs :: [Def]
defs = mkDefs

  [ ("+", binop Builtin.Add)
  , ("-", binop Builtin.Sub)
  , ("*", binop Builtin.Mul)
  , ("%", binop Builtin.ModInt)
  , ("==", binop Builtin.EqNumOrChar)
  , ("<", binop Builtin.LessNumOrChar)
  , ("eqString", binop Builtin.EqString)
  , ("true", ECon $ Builtin.Bool True)
  , ("false", ECon $ Builtin.Bool False)
  , ("^", binop Builtin.StringAppend)
  , ("showChar", unop Builtin.ShowChar)
  , ("showInt", unop Builtin.ShowInt)
  , ("readInt", unop Builtin.ReadInt)
  , ("argv", unop Builtin.Argv)
  , ("size", unop Builtin.StrSize)
  , ("index", binop Builtin.StrIndex)
  , ("error", unop Builtin.Error)
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
