
-- The language as it comes out of the Parser.
module Rep_Ast (Var(..),Exp(..),Def(..),Value(..),Env,env0) where

import Builtin
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

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


prim2value :: Builtin.Prim2 -> Value
prim2value prim = abs x (ELam y (EPrim2 prim (EVar x) (EVar y)))
  where
    abs = Clo Map.empty
    x = Var "x"
    y = Var "y"

env0 :: Env
env0 = Map.fromList
  [ (Var "+", prim2value Builtin.Add)
  , (Var "-", prim2value Builtin.Sub)
  ]
