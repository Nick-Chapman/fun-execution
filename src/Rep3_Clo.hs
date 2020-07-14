
-- | Closure Converted Code

module Rep3_Clo (Loc(..),Atom(..),Code(..),Value(..)) where

import qualified Builtin

data Loc = LocArg Int | LocFree Int

data Atom = ALoc Loc | ACon Builtin.BV

data Code
  = Return Atom
  | Tail Atom [Atom]
  | LetContinue { freeFollow :: [Loc], rhs :: Code, follow :: Code }
  | LetPrim1 Builtin.Prim1 Atom Code
  | LetPrim2 Builtin.Prim2 (Atom,Atom) Code
  | LetClose { freeBody :: [Loc], arity :: Int, body :: Code, code :: Code }
  | Branch Atom Code Code

data Value
  = Base Builtin.BV
  | Clo {fvs :: [Value], arity :: Int, body :: Code }

instance Show Value where
  show = \case
    Base bv -> show bv
    Clo{} -> "<closure>"

instance Show Loc where
  show = \case
    LocArg n -> "*" ++ show n
    LocFree n -> "~" ++ show n

instance Show Atom where show = \case ALoc x -> show x; ACon v -> show v
instance Show Code where show = unlines . pretty

pretty :: Code -> [String]
pretty = \case
  Return a ->
    ["return: " ++ show a]
  Tail func args ->
    ["tail: " ++ show func ++ " " ++ show args]
  LetContinue{freeFollow,rhs,follow} ->
    indented ("push-k: " ++ show freeFollow ++  " ->") (pretty follow)
    ++ pretty rhs
  LetPrim1 prim a1 code ->
    ["let-op: " ++ show prim ++ " " ++ show a1]
    ++ pretty code
  LetPrim2 prim (a1,a2) code ->
    ["let-op: " ++ show prim ++ " " ++ show (a1,a2)]
    ++ pretty code
  LetClose{freeBody,arity,body,code} ->
    indented ("let-close: " ++ show freeBody ++ " \\" ++ show arity ++ ".") (pretty body)
    ++ pretty code
  Branch a1 c2 c3 ->
    ["if " ++ show a1]
    ++ indented "then" (pretty c2)
    ++ indented "else" (pretty c3)

indented :: String -> [String] -> [String]
indented hang = \case
  [] -> error "indented, no body"
  [oneLine] -> [hang ++ " " ++ oneLine]
  lines -> [hang] ++ ["  " ++ line | line <- lines]

