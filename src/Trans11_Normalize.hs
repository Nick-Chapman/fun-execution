
-- | Normalize an expression, using NbE ("normalization by evaluation")

module Trans11_Normalize(normalize) where

import Control.Monad (ap,liftM,(>=>))
import Data.Map (Map)
import Rep1_Ast as Ast (Var(..),Exp(..),mkELam,mkEApp)
import qualified Data.Map.Strict as Map

normalize :: Exp -> Exp
normalize = runM . norm

norm :: Exp -> M Exp
norm = reflect >=> reify

data SemVal = Syntax Exp | Macro String (SemVal -> M SemVal)

reify :: SemVal -> M Exp
reify = \case
  Syntax e -> return e
  Macro tag f -> do
    x <- Fresh tag
    body <- Reset (f (Syntax (EVar x)) >>= reify)
    return $ mkELam x body

apply :: SemVal -> SemVal -> M SemVal
apply func arg = case func of
  Syntax func -> do
    arg <- reify arg
    return $ Syntax $ mkEApp func arg
  Macro tag f -> do
    if duplicatable arg then f arg else do -- beta!
      x <- Fresh tag
      arg <- reify arg
      Wrap (ELet x arg) (f (Syntax (EVar x)))

duplicatable :: SemVal -> Bool
duplicatable = \case
  Syntax (ECon _) -> True
  Syntax (EVar _) -> True
  Syntax _ -> False
  Macro{} -> True

reflect :: Exp -> M SemVal
reflect = \case

  ECon bv -> do
    return $ Syntax (ECon bv)

  EPrim1 prim e1 -> do
    e1 <- norm e1
    return $ Syntax $ EPrim1 prim e1

  EPrim2 prim e1 e2 -> do
    e1 <- norm e1
    e2 <- norm e2
    return $ Syntax $ EPrim2 prim e1 e2

  EVar x -> do
    Lookup x

  ELam [] body -> reflect body

  ELam (x:xs) body -> do
    env <- Save
    return $ Macro (unVar x) $ \arg -> do
      Restore env $ ModEnv (Map.insert x arg) $ reflect (ELam xs body)

  EApp func [] -> reflect func

  EApp func [arg] -> do
    func <- reflect func
    arg <- reflect arg
    apply func arg

  EApp func (arg:args) -> reflect (EApp (EApp func [arg]) args)

  ELet x rhs body ->
    reflect (EApp (ELam [x] body) [rhs])

  EFix f body -> do
    f' <- Fresh (unVar f)
    body <- ModEnv (Map.insert f (Syntax (EVar f'))) $ Reset (norm body)
    return $ Syntax $ EFix f' body

  EIf e1 e2 e3 -> do
    e1 <- norm e1
    e2 <- Reset (norm e2)
    e3 <- Reset (norm e3)
    return $ Syntax $ EIf e1 e2 e3


instance Functor M where fmap = liftM
instance Applicative M where pure = return; (<*>) = ap
instance Monad M where return = Ret; (>>=) = Bind

data M a where
  Ret :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  Lookup :: Var -> M SemVal
  ModEnv :: (Env -> Env) -> M a -> M a
  Fresh :: String -> M Var
  Save :: M Env
  Restore :: Env -> M a -> M a
  Wrap :: (Exp -> Exp) -> M a -> M a
  Reset :: M Exp -> M Exp

type Env = Map Var SemVal

runM :: M Exp -> Exp
runM m = snd $ loop Map.empty 1 m k0 where
  k0 s e = (s,e)
  loop :: Env -> State -> M a -> (State -> a -> Res) -> Res
  loop env state m k = case m of

    Ret x -> k state x
    Bind m f -> loop env state m $ \state a -> loop env state (f a) k
    Lookup x -> maybe (error $ "lookup:"<>show x) (k state) (Map.lookup x env)
    Fresh tag -> k (state+1) $ Var (tag ++ show state)
    Save -> k state env
    Restore env m -> loop env state m k
    ModEnv f m -> loop (f env) state m k
    Wrap f m -> f' (loop env state m k) where f' (s,e) = (s,f e)
    Reset m -> let (state',v) = loop env state m k0 in k state' v

type Res = (State,Exp)
type State = Int
