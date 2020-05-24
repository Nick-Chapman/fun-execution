
module Close_Ast (UnboundError,closeOverEnvironment) where

import Control.Monad(ap,liftM)
import qualified Data.Map.Strict as Map

import Rep_Ast(Var(..),Exp(..),Env)

data UnboundError = UnboundError { unUnboundError :: String }
instance Show UnboundError where show = unUnboundError

closeOverEnvironment :: Exp -> Env -> Either UnboundError Exp
closeOverEnvironment exp env = runM env (close exp)

close :: Exp -> M Exp
close = \case
  e@ECon{} -> do
    return e
  EPrim2 prim e1 e2 -> do
    e1 <- close e1
    e2 <- close e2
    return $ EPrim2 prim e1 e2
  EVar x -> do
    Lookup x
  ELam x body -> do
    body <- Extend x $ close body
    return $ ELam x body
  EApp e1 e2 -> do
    e1 <- close e1
    e2 <- close e2
    return $ EApp e1 e2
  ELet x rhs body -> do
    rhs <- close rhs
    body <- Extend x $ close body
    return $ ELet x rhs body

instance Functor M where fmap = liftM
instance Applicative M where pure = return; (<*>) = ap
instance Monad M where return = Ret; (>>=) = Bind

data M a where
  Ret :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  Extend :: Var -> M a -> M a
  Lookup :: Var -> M Exp

runM :: Env -> M a -> Either UnboundError a
runM env = loop [] where
  loop :: [Var] -> M a -> Either UnboundError a
  loop xs = \case
    Ret x -> return x
    Bind m f -> do v <- loop xs m; loop xs (f v)
    Extend x m -> loop (x:xs) m
    Lookup x ->
      if x `elem` xs then return $ EVar x else
        case Map.lookup x env of
          Just v -> return $ ECon v
          Nothing -> Left $ UnboundError $ "unbound variable: " <> show x
