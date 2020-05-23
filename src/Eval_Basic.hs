
module Eval_Basic (RuntimeError,evaluate,env0) where

import Control.Monad(ap,liftM)
import qualified Data.Map.Strict as Map

import Rep_Basic(Var(..),Exp(..),Value(..),Env)
import qualified Builtin

data RuntimeError = RuntimeError { unRuntimeError :: String }
instance Show RuntimeError where show = unRuntimeError

evaluate :: Env -> Exp -> Either RuntimeError Value
evaluate env exp = runM env (eval exp)

eval :: Exp -> M Value
eval = \case
  ENum n -> do
    return $ Base $ Builtin.Num n
  EStr s -> do
    return $ Base $ Builtin.Str s
  EVar x -> do
    Lookup x
  ELam x body -> do
    env <- Save
    return $ Clo env x body
  EApp e1 e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    apply v1 v2
  ELet x rhs body -> do
    eval (EApp (ELam x body) rhs)

apply :: Value -> Value -> M Value
apply = \case
  Clo env x body ->
    \arg -> Restore (Map.insert x arg env) $ eval body
  Prim2 prim -> \case
    Base bv -> return $ Prim2_1 prim bv
    v -> Err $ "cant apply primitive to non-base arg1: " <> show (prim,v)
  Prim2_1 prim bv1 -> \case
    Base bv2 -> Base <$> (returnOrError $ Builtin.apply2 prim (bv1,bv2))
    v2 -> Err $ "cant apply primitive to non-base arg2: " <> show (prim,bv1,v2)
  v ->
    \_ -> Err $ "cant apply non-function: " <> show v

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

runM :: Env -> M Value -> Either RuntimeError Value
runM = loop where
  loop :: Env -> M a -> Either RuntimeError a
  loop env = \case
    Ret x -> return x
    Bind m f -> do v <- loop env m; loop env (f v)
    Err msg -> err msg
    Lookup x -> maybe (err $ "lookup:" <> show x) Right $ Map.lookup x env
    Save -> return env
    Restore env m -> loop env m

  err = Left . RuntimeError


env0 :: Env
env0 = Map.fromList
  [ (Var "+", Prim2 Builtin.Add)
  , (Var "-", Prim2 Builtin.Sub)
  ]
