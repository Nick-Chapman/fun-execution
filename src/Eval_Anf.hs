
module Eval_Anf (Value,Env,evaluate) where

import Control.Monad.Identity (Identity(..))
import Rep_Anf(Var(..),Code)
import qualified Rep_Anf as Anf
import qualified Builtin

import Data.Map (Map,insert)
import qualified Data.Map.Strict as Map

data Value
  = Base Builtin.BV
  | Clo Env Var Code

instance Show Value where
  show = \case
    Base bv -> show bv
    Clo{} -> "<closure>"

type M a = Identity a

type Machine {-m-} = (Control,Env,Kont)
type Control {-c-} = Code
type Env     {-q-} = Map Var Value
data Kont    {-k-} = Kbind Env Var Code Kont | Kdone

-- | execute (flat)code on a CEK machine
evaluate :: Env -> Code -> Value
evaluate env code = runIdentity $ run (code, env, Kdone)

-- | run a machine unti the final value is calculated
run :: Machine -> M Value
run (c,q,k) = case c of
  Anf.Return a -> do
    v <- atomic q a
    ret v k

  Anf.Tail f a -> do
    func <- atomic q f
    arg <- atomic q a
    enter func arg k

  Anf.LetCode x rhs body -> do
    k <- pure $ Kbind q x body k
    run (rhs,q,k)

  Anf.LetOp x op (a1,a2) c -> do
    v1 <- atomic q a1
    v2 <- atomic q a2
    v <- doOp op v1 v2
    q <- pure $ insert x v q
    run (c,q,k)

  Anf.LetLam x (fx,fc) c -> do
    q <- pure $ insert x (Clo q fx fc) q
    run (c,q,k)

  Anf.Branch a1 c2 c3 -> do
    v <- atomic q a1
    code <- branch c2 c3 v
    run (code,q,k)

ret :: Value -> Kont -> M Value
ret v = \case
  Kdone -> return v
  Kbind q x c k -> run (c,q',k) where q' = insert x v q

enter :: Value -> Value -> Kont -> M Value
enter func arg k = case func of
  Base{} -> err "cant enter a non-function"
  Clo q x body -> run (body, q', k) where q' = insert x arg q

branch :: Code -> Code -> Value -> M Code
branch c2 c3 = \case
  Base (Builtin.Bool True) -> return c2
  Base (Builtin.Bool False) -> return c3
  _ -> err "cant branch on a non boolean"

atomic :: Env -> Anf.Atom -> M Value
atomic q = \case
  Anf.AVar x -> look x q
  Anf.ACon bv -> return $ Base bv

look :: Var -> Env -> M Value
look x q = maybe (err $ "runtime-lookup: " ++ show x) return (Map.lookup x q)

doOp :: Builtin.Prim2 -> Value -> Value -> M Value
doOp prim = \case
  Clo{} -> \_ -> err $ "cant apply primitive to arg1-closure: " <> show prim
  Base bv1 -> \case
    Clo{} -> err $ "cant apply primitive to arg2-closure: " <> show (prim,bv1)
    Base bv2 -> Base <$> (returnOrError $ Builtin.apply2 prim (bv1,bv2))

returnOrError :: Show e => Either e a -> M a
returnOrError = \case
  Left e -> err $ show e
  Right a -> return a

err :: String -> M a
err msg = error $ "runtime-error: " ++ msg
