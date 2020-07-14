
-- | AST for lambda expressions

module Rep1_Ast (Var(..),Exp(..),Def(..),indented,pretty,mkELam,mkEApp,wrapDef) where

import Data.Set (Set,(\\))
import qualified Builtin
import qualified Data.Char as Char
import qualified Data.Set as Set

newtype Var = Var { unVar :: String } deriving (Eq,Ord)
instance Show Var where
  show (Var s) = if symbolic s then "("++s++")" else s

symbolic :: String -> Bool
symbolic = any symbolicChar where
  symbolicChar c = not (Char.isAlpha c || Char.isDigit c || c == '\'' || c == '_')

data Def = Def Var Exp deriving (Show)

data Exp
  = ECon Builtin.BV
  | EPrim1 Builtin.Prim1 Exp
  | EPrim2 Builtin.Prim2 Exp Exp
  | EVar Var
  | ELam [Var] Exp
  | EApp Exp [Exp]
  | ELet Var Exp Exp
  | EIf Exp Exp Exp
  | EFix Var Exp

instance Show Exp where show = unlines . pretty

type Lines = [String]

pretty :: Exp -> Lines
pretty = \case
  ECon v -> [show v]
  EVar x -> [show x]
  EPrim1 prim e1 -> bracket (indented (show prim) (pretty e1))
  EPrim2 prim e1 e2 -> bracket (indented (show prim) (jux (pretty e1) (pretty e2)))
  EApp func args -> bracket (foldl jux [] (map pretty (func:args)))
  ELam xs body -> bracket (indented ("\\" ++ show xs ++ ".") (pretty body))
  ELet x rhs body -> indented ("let " ++ show x ++ " =") (pretty rhs) ++ pretty body
  EFix x body -> bracket (indented ("fix " ++ show x ++ " in") (pretty body))
  EIf i t e ->
    bracket (indented "if" (pretty i) ++ indented "then" (pretty t) ++ indented "else" (pretty e))

bracket :: Lines -> Lines
bracket = onHead ("(" ++) . onTail (++ ")")

onHead,onTail :: (String -> String) -> Lines -> Lines
onHead _ [] = error "onHead"
onHead f (x:xs) = f x : xs
onTail f = reverse . onHead f . reverse

jux :: Lines -> Lines -> Lines
jux [x] [y] = [ x ++ " " ++ y ]
jux xs ys = xs ++ ys

indented :: String -> Lines -> Lines
indented hang = \case
  [] -> error "indented"
  [oneLine] -> [hang ++ " " ++ oneLine]
  lines -> [hang] ++ ["  " ++ line | line <- lines]

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


wrapDef :: Def -> Exp -> Exp
wrapDef (Def x rhs) body =
  -- we dont wrap the def if it does not occur in the body, regardless
  -- of the potential computation effect of the rhs
  if x `elem` fvs body
  then ELet x rhs body
  else body

fvs :: Exp -> Set Var
fvs = \case
  ECon _ -> Set.empty
  EPrim1 _ e1 -> fvs e1
  EPrim2 _ e1 e2 -> Set.union (fvs e1) (fvs e2)
  EVar x -> Set.fromList [x]
  ELam xs body -> fvs body \\ Set.fromList xs
  EApp func args -> Set.unions (map fvs (func:args))
  ELet x rhs body -> Set.union (fvs rhs) (fvs body \\ Set.fromList [x])
  EIf i t e -> Set.unions (map fvs [i,t,e])
  EFix x body -> fvs body \\ Set.fromList [x]
