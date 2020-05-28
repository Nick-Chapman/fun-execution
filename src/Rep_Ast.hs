
-- The language as it comes out of the Parser.
module Rep_Ast (Var(..),Exp(..),Def(..),Env,indented,pretty,env0,mkELam,mkEApp,wrapDef) where

import Data.Set (Set,(\\))
import qualified Data.Char as Char
import qualified Data.Set as Set

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Builtin

newtype Var = Var { unVar :: String } deriving (Eq,Ord)
instance Show Var where
  show (Var s) = if symbolic s then "("++s++")" else s

symbolic :: String -> Bool
symbolic = any symbolicChar where
  symbolicChar c = not (Char.isAlpha c || Char.isDigit c || c == '\'' || c == '_')


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

instance Show Exp where show = unlines . pretty

type Lines = [String]

-- | multi-line pretty print
pretty :: Exp -> Lines
pretty = \case
  ECon v -> [show v]
  EVar x -> [show x]
  EPrim2 prim e1 e2 -> indented (show prim) (jux (pretty e1) (pretty e2))
  EApp func args -> bracket (foldl jux [] (map pretty (func:args)))
  ELam xs body -> indented ("\\" ++ show xs ++ ".") (pretty body)
  ELet x rhs body -> indented ("let " ++ show x ++ " =") (pretty rhs) ++ pretty body
  EFix x body -> indented ("fix " ++ show x ++ " in") (pretty body)
  EIf i t e -> indented "if" (pretty i) ++ indented "then" (pretty t) ++ indented "else" (pretty e)

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


type Env = Map Var Exp

binop :: Builtin.Prim2 -> Exp
binop prim = mkELam x (mkELam y (EPrim2 prim (EVar x) (EVar y)))
  where
    x = Var "x"
    y = Var "y"

y :: Exp
y = mkELam unfixed (EFix f (mkEApp (EVar unfixed) (EVar f)))
  where
    unfixed = Var "F"
    f = Var "f"

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
  EPrim2 _ e1 e2 -> Set.union (fvs e1) (fvs e2)
  EVar x -> Set.fromList [x]
  ELam xs body -> fvs body \\ Set.fromList xs
  EApp func args -> Set.unions (map fvs (func:args))
  ELet x rhs body -> Set.union (fvs rhs) (fvs body \\ Set.fromList [x])
  EIf i t e -> Set.unions (map fvs [i,t,e])
  EFix x body -> fvs body \\ Set.fromList [x]
