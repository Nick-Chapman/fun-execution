
-- | Flatten an AST expression to ANF code

module Trans12_Ast2Anf(flatten) where

import Control.Monad (ap,liftM,forM)
import Data.Map (Map)
import Rep1_Ast as Ast (Exp(..),Var(..))
import Rep2_Anf as Anf (Code(..),Atom(..))
import qualified Data.Map.Strict as Map

flatten :: Exp -> Code
flatten exp = runM (codifyAs Nothing exp)

codifyAs :: Maybe Var -> Exp -> M Code
codifyAs mx = \case
  ECon bv -> do
    return $ Return $ ACon bv
  EPrim1 prim e1 -> do
    a1 <- atomize $ codify e1
    name <- fresh mx
    Wrap (LetPrim1 name prim a1) (return $ Return $ AVar name)
  EPrim2 prim e1 e2 -> do
    a1 <- atomize $ codify e1
    a2 <- atomize $ codify e2
    name <- fresh mx
    Wrap (LetPrim2 name prim (a1,a2)) (return $ Return $ AVar name)
  EVar x -> do
    a <- Lookup x
    return $ Return a
  ELam formals body -> do
    let bodyName = fmap (suffix "-body") mx
    name <- fresh mx
    let mod = Map.union (Map.fromList [ (x,AVar x) | x <- formals ])
    body <- ModEnv mod $ Reset (codifyAs bodyName body)
    Wrap (LetLam name (formals,body)) (return $ Return $ AVar name)
  EApp func args -> do
    aFunc <- atomize $ codify func
    aArgs <- forM args $ (atomize . codify)
    return $ Tail aFunc aArgs
  ELet x rhs body -> do
    a <- atomizeAs (Just x) $ codifyAs (Just x) rhs
    ModEnv (Map.insert x a) $ codifyAs mx body
  EIf e1 e2 e3 -> do
    let thenName = fmap (suffix "-then") mx
    let elseName = fmap (suffix "-else") mx
    a1 <- atomize $ codify e1
    c2 <- Reset (codifyAs thenName e2)
    c3 <- Reset (codifyAs elseName e3)
    return $ Branch a1 c2 c3
  EFix f body0 -> do
    let (xs,body) = case body0 of ELam xs body -> (xs,body); _ -> ([],body0)
    let mod = Map.union (Map.fromList [ (x,AVar x) | x <- f:xs ])
    body <- ModEnv mod $ Reset (codify body)
    Wrap (LetFix f (xs,body)) (return $ Return $ AVar f)

  where
    codify = codifyAs Nothing
    atomize = atomizeAs Nothing
    suffix ext (Var base) = Var $ base ++ ext

atomizeAs :: Maybe Var -> M Code -> M Atom
atomizeAs mx m = do
  m >>= \case
    Return a -> return a -- dont re-name at atom
    rhs -> do
      x <- fresh mx
      Wrap (LetCode x rhs) $ return $ AVar x

{-
-- | Avoid pushing a continutaion which calls a known function
letCode :: Var -> Code -> Code -> Code
letCode x rhs body
  | Tail (AVar x') arg <- body, x==x', LetLam f def (Return (AVar f')) <- rhs, f==f' =
  {-
      push: \x. tail: x arg in
      let f = \<def> in
      return: f
  -->
      let f = \<def> in
      tail: f arg
  -}
      LetLam f def (Tail (AVar f) arg)
  | otherwise =
      LetCode x rhs body
-}


fresh :: Maybe Var -> M Var
fresh = \case
  Just var -> return var
  Nothing -> Fresh


instance Functor M where fmap = liftM
instance Applicative M where pure = return; (<*>) = ap
instance Monad M where return = Ret; (>>=) = Bind

data M a where
  Ret :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  Fresh :: M Var
  Reset :: M Code -> M Code
  Wrap :: (Code -> Code) -> M a -> M a
  ModEnv :: (CompileEnv -> CompileEnv) -> M a -> M a
  Lookup :: Var -> M Atom

runM :: M Code -> Code
runM m = snd $ loop Map.empty 1 m k0 where
  k0 state code = (state,code)
  loop :: CompileEnv -> State -> M a -> (State -> a -> Res) -> Res
  loop env state m k = case m of

    Ret x -> k state x
    Bind m f -> loop env state m $ \state a -> loop env state (f a) k
    Reset m -> let (state',code) = loop env state m k0 in k state' code
    Wrap f m -> f' (loop env state m k) where f' (s,a) = (s,f a)
    Lookup x -> k state (lookup x env)
    ModEnv f m -> loop (f env) state m k
    Fresh -> k (state+1) (Var ("_g" <> show state))

  lookup :: Var -> CompileEnv -> Atom
  lookup x env =
    case Map.lookup x env of
      Just atom -> atom
      Nothing -> AVar x

type Res = (State,Code)
type State = Int
type CompileEnv = Map Var Atom
