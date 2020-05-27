
-- The language as it comes out of the Parser.
module Rep_Ast (Var(..),Exp(..),mkELam,mkEApp,Def(..),Env,env0) where

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
  | ELam [Var] Exp
  | EApp Exp [Exp]
  | ELet Var Exp Exp
  | EIf Exp Exp Exp
  | EFix Var Exp

instance Show Exp where
  show = \case
    ECon v -> show v
    EPrim2 prim e1 e2 -> show prim ++ show (e1,e2)
    EVar s -> show s
    ELam x body -> "(\\" ++ show x ++ "." ++ show body ++ ")"
    EApp e1 e2 -> "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    ELet x e1 e2 -> "(let " ++ show x ++ " = " ++ show e1 ++ " in " ++ show e2 ++ ")"
    EIf i t e -> "(if " ++ show i ++ " then " ++ show t ++ " else " ++ show e ++ ")"
    EFix x e -> "(fix " ++ show x ++ " in " ++ show e ++ ")"

type Env = Map Var Exp

binop :: Builtin.Prim2 -> Exp
binop prim = mkELam x (mkELam y (EPrim2 prim (EVar x) (EVar y)))
  where
    x = Var "x"
    y = Var "y"

y :: Exp
y = mkELam f (EFix a (mkEApp (EVar f) (EVar a)))
  where
    f = Var "fff"
    a = Var "aaa"

env0 :: Env
env0 = Map.fromList
  [ (Var "+", binop Builtin.Add)
  , (Var "-", binop Builtin.Sub)
  , (Var "*", binop Builtin.Mul)
  , (Var "%", binop Builtin.ModInt)
  , (Var "==", binop Builtin.EqInt)
  , (Var "<", binop Builtin.LessInt)
  , (Var "true", ECon $ Builtin.Bool True)
  , (Var "false", ECon $ Builtin.Bool False)
  , (Var "y", y)
  ]


-- smart constructors
doMultiLam,doMultiApp :: Bool
doMultiLam = True
doMultiApp = True

mkELam :: Var -> Exp -> Exp
mkELam x = \case
  ELam xs exp | doMultiLam -> ELam (x:xs) exp
  exp -> ELam [x] exp

mkEApp :: Exp -> Exp -> Exp
mkEApp fun arg = case fun of
  EApp fun args | doMultiApp -> EApp fun (args ++ [arg])
  _ -> EApp fun [arg]
