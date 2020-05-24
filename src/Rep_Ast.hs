
-- The language as it comes out of the Parser.
module Rep_Ast (Var(..),Exp(..),Def(..),Value(..),Env) where

import Data.Map.Strict (Map)
import Builtin

newtype Var = Var { unVar :: String } deriving (Eq,Ord)
instance Show Var where show = unVar

data Def = Def Var Exp deriving (Show)

-- Expressions... Embedded values; Saturated prim-ops; TODO: Multi Lam/App

data Exp
  = ECon Value
  | EPrim2 Prim2 Exp Exp
  | EVar Var
  | ELam Var Exp
  | EApp Exp Exp
  | ELet Var Exp Exp
  deriving (Show)

type Env = Map Var Value

data Value
  = Base BV
  | Clo Env Var Exp

instance Show Value where
  show = \case
    Base bv -> show bv
    Clo{} -> "<closure>"
