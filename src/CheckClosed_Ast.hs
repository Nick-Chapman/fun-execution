
module CheckClosed_Ast (UnboundError,checkInEnv) where

import Control.Monad(ap,liftM)

import Rep_Ast(Var(..),Exp(..))

data UnboundError = UnboundError { unUnboundError :: String }
instance Show UnboundError where show = unUnboundError

checkInEnv :: [Var] -> Exp -> Maybe UnboundError
checkInEnv xs exp =
  case runM xs (check exp) of
    Left err -> Just err
    Right () -> Nothing

check :: Exp -> M ()
check = \case
  ECon{} -> do
    return ()
  EPrim2 _ e1 e2 -> do
    check e1
    check e2
  EVar x -> do
    Check x
  ELam xs body -> do
    Extend xs $ check body
  EApp fun args -> do
    check fun
    mapM_ check args
  ELet x rhs body -> do
    check rhs
    Extend [x] $ check body
  EIf i t e -> do
    check i
    check t
    check e

instance Functor M where fmap = liftM
instance Applicative M where pure = return; (<*>) = ap
instance Monad M where return = Ret; (>>=) = Bind

data M a where
  Ret :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  Extend :: [Var] -> M a -> M a
  Check :: Var -> M ()

runM :: [Var] -> M () -> Either UnboundError ()
runM = loop where
  loop :: [Var] -> M a -> Either UnboundError a
  loop xs = \case
    Ret x -> return x
    Bind m f -> do v <- loop xs m; loop xs (f v)
    Extend xs' m -> loop (xs'++xs) m
    Check x ->
      if x `elem` xs then return () else
        Left $ UnboundError $ "unbound variable: " <> show x
