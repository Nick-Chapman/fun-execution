
-- The basic internal expression language. Minimal forms. Saturated builtins. Multi lam/apps
-- Is this where NbE should take place?
module Rep_Basic(Var(..),Exp(..),Value(..),Env) where

import Data.Map.Strict (Map)

import Builtin(BV(..),Prim2)
import Rep_Lang(Var(..),Exp(..))

-- Soon Basic.Exp will not be the same as Lang.Exp
-- data Exp = ...

type Env = Map Var Value

data Value
  = Base BV
  | Prim2 Prim2
  | Prim2_1 Prim2 BV
  | Clo Env Var Exp

instance Show Value where
  show = \case
    Base bv -> show  bv
    Prim2 prim -> show prim
    Prim2_1 prim bv -> show (prim,bv)
    Clo{} -> "<closure>"
