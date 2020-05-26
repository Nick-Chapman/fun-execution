
-- Flattened (ANF style) code
module Rep_Anf(Var(..),Atom(..),Code(..)) where

import qualified Builtin
import Rep_Ast(Var(..))

data Atom = AVar Var | ACon Builtin.BV

data Code -- flattened expression
  = Return Atom
  | Tail Atom Atom
  | LetCode Var Code Code
  | LetOp Var Builtin.Prim2 (Atom,Atom) Code
  | LetLam Var (Var,Code) Code
  | Branch Atom Code Code

instance Show Atom where show = \case AVar s -> show s; ACon v -> show v
instance Show Code where show = unlines . pretty

-- | pretty print, showing explicit continutaion management: push, return, (and tail)
pretty :: Code -> [String]
pretty = \case
  Return a -> ["return: " ++ show a]
  Tail xf a -> ["tail: " ++ show xf ++ " " ++ show a]
  LetCode x rhs body -> indented ("push: " ++ show x ++  " ->") (pretty body) ++ pretty rhs
  LetOp x op (a1,a2) c -> indented ("let " ++ show x ++ " =") [show (op,a1,a2)] ++ pretty c
  LetLam y (x,body) c -> indented ("let " ++ show y ++ " = \\" ++ show x ++ ".") (pretty body) ++ pretty c
  Branch a1 c2 c3 -> ["if " ++ show a1] ++ indented "then" (pretty c2) ++ indented "else" (pretty c3)

indented :: String -> [String] -> [String]
indented hang = \case
  [] -> error "indented, no body"
  [oneLine] -> [hang ++ " " ++ oneLine]
  lines -> [hang] ++ ["  " ++ line | line <- lines]
