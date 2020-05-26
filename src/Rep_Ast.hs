
-- The language as it comes out of the Parser.
module Rep_Ast (Var(..),Exp(..),Def(..),Env,env0) where

import qualified Builtin
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

newtype Var = Var { unVar :: String } deriving (Eq,Ord)
instance Show Var where show = unVar

data Def = Def Var Exp deriving (Show)

-- Expressions... Saturated prim-ops; TODO: Multi Lam/App

data Exp
  = ECon Builtin.BV
  | EPrim2 Builtin.Prim2 Exp Exp
  | EVar Var
  | ELam Var Exp
  | EApp Exp Exp
  | ELet Var Exp Exp

instance Show Exp where
  show = \case
    ECon v -> show v
    EPrim2 prim e1 e2 -> show prim ++ show (e1,e2)
    EVar s -> show s
    ELam x body -> "(\\" ++ show x ++ "." ++ show body ++ ")"
    EApp e1 e2 -> "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    ELet x e1 e2 -> "(let " ++ show x ++ " = " ++ show e1 ++ " in " ++ show e2 ++ ")"

type Env = Map Var Exp

prim2value :: Builtin.Prim2 -> Exp
prim2value prim = ELam x (ELam y (EPrim2 prim (EVar x) (EVar y)))
  where
    x = Var "xx"
    y = Var "yy"

env0 :: Env
env0 = Map.fromList
  [ (Var "+", prim2value Builtin.Add)
  , (Var "-", prim2value Builtin.Sub)
  ]
