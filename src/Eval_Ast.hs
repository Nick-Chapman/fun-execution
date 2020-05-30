
-- | Evaluate AST expressions directly

module Eval_Ast (Value,evaluate) where

import Control.Monad(ap,liftM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Rep_Ast(Var(..),Exp(..))
import qualified Builtin
import Parse (parse)

data Value = Base Builtin.BV | Clo Env Var Exp
type Env = Map Var Value

instance Show Value where
  show = \case
    Base bv -> show bv
    Clo{} -> "<closure>"

evaluate :: Exp -> Value
evaluate exp = runM (eval exp)

eval :: Exp -> M Value
eval = \case
  ECon v -> do
    return $ Base v
  EPrim1 prim e1 -> do
    v1 <- eval e1
    applyPrim1 prim v1
  EPrim2 prim e1 e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    applyPrim2 prim v1 v2
  EVar x -> do
    Lookup x
  ELam [] body -> eval body
  ELam (x:xs) body -> do
    env <- Save
    return $ Clo env x (ELam xs body)
  EApp e1 [] -> eval e1
  EApp e1 [e2] -> do
    v1 <- eval e1
    v2 <- eval e2
    apply v1 v2
  EApp e1 (e2:rest) -> eval (EApp (EApp e1 [e2]) rest)
  ELet x rhs body -> do
    eval (EApp (ELam [x] body) [rhs])
  EIf i t e -> do
    i <- eval i
    branch <- ite i t e
    eval branch
  EFix x body -> do
    eval (EApp y [ELam [x] body])

y :: Exp
y = parseExp "\\f. (\\x. f (\\v. x x v)) (\\x. f (\\v. x x v))"

parseExp :: String -> Exp
parseExp =
  either (error . show) id
  . maybe (error "parseExp:None") id
  . either (error . show) id
  . parse

apply :: Value -> Value -> M Value
apply = \case
  Clo env x body ->
    \arg -> Restore (Map.insert x arg env) $ eval body
  v ->
    \_ -> Err $ "cant apply non-function: " <> show v

applyPrim1 :: Builtin.Prim1 -> Value -> M Value
applyPrim1 prim = \case
  Clo{} -> Err $ "cant apply prim1 to arg1-closure: " <> show prim
  Base bv1 -> Base <$> (returnOrError $ Builtin.apply1 prim bv1)

applyPrim2 :: Builtin.Prim2 -> Value -> Value -> M Value
applyPrim2 prim = \case
  Clo{} -> \_ -> Err $ "cant apply prim2 to arg1-closure: " <> show prim
  Base bv1 -> \case
    Clo{} -> Err $ "cant apply primitive to arg2-closure: " <> show (prim,bv1)
    Base bv2 -> Base <$> (returnOrError $ Builtin.apply2 prim (bv1,bv2))

ite :: Value -> Exp -> Exp -> M Exp
ite = \case
  Base (Builtin.Bool True) -> \t _ -> return t
  Base (Builtin.Bool False) -> \_ e -> return e
  i -> \_ _ -> Err $ "cant test non-boolean if-then-else condition: " <> show i

returnOrError :: Show e => Either e a -> M a
returnOrError = \case
  Left e -> Err $ show e
  Right a -> return a

instance Functor M where fmap = liftM
instance Applicative M where pure = return; (<*>) = ap
instance Monad M where return = Ret; (>>=) = Bind

data M a where
  Ret :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  Err :: String -> M a
  Lookup :: Var -> M Value
  Save :: M Env
  Restore :: Env -> M a -> M a

runM :: M Value -> Value
runM = loop Map.empty where
  loop :: Env -> M a -> a
  loop env = \case
    Ret x -> x
    Bind m f -> loop env (f (loop env m))
    Err msg -> err msg
    Lookup x -> maybe (err $ "lookup:" <> show x) id $ Map.lookup x env
    Save -> env
    Restore env m -> loop env m

  err msg = error $ "runtime-error: " ++ msg

