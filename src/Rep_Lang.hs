
-- The user level language as it comes out of the Parser.
module Rep_Lang (Var(..),Exp(..),Def(..)) where

newtype Var = Var { unVar :: String } deriving (Eq,Ord)
instance Show Var where show = unVar

data Def = Def Var Exp deriving (Show)

data Exp
  = ENum Int
  | EStr String
  | EVar Var
  | ELam Var Exp
  | EApp Exp Exp
  | ELet Var Exp Exp
  deriving (Show)
