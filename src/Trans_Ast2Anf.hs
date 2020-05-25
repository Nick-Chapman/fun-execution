
module Trans_Ast2Anf(flatten,flattenV) where

import Control.Monad(ap,liftM)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

import Rep_Ast as Ast(Exp(..),Var(..))
import Rep_Anf as Anf(Code(..),Atom(..),Env)
import qualified Rep_Ast as Ast
import qualified Rep_Anf as Anf

-- | compile an expression to (flat)code for a CEK machine
flatten :: Env -> Exp -> Code
flatten consoleEnv exp = flattenE consoleEnv Map.empty exp

flattenE :: Env -> CompileEnv -> Exp -> Code
flattenE consoleEnv env exp =
  runM consoleEnv env (codifyAs Nothing exp)


flattenV :: Ast.Value -> Anf.Value
flattenV = \case
  Ast.Base bv -> Anf.Base bv
  Ast.Clo env x exp -> do
    let env' :: Anf.Env = Map.map flattenV env
    let code = flattenE env' (Map.insert x (AVar x) Map.empty) exp
    Anf.Clo env' x code

flattenM :: Ast.Value -> M Anf.Value
flattenM = \case
  Ast.Base bv -> return $ Anf.Base bv
  Ast.Clo env x exp -> do
    let env' :: Anf.Env = Map.map flattenV env
    code <- ModEnv (\_ -> Map.insert x (AVar x) $ Map.map ACon env') $ codifyAs Nothing exp
    return $ Anf.Clo env' x code

codifyAs :: Maybe Var -> Exp -> M Code
codifyAs mx = \case
  ECon v -> do
    v <- flattenM v
    return $ Return $ ACon v
  EPrim2 op e1 e2 -> do
    a1 <- atomize $ codify e1
    a2 <- atomize $ codify e2
    name <- fresh mx
    Wrap (LetOp name op (a1,a2)) (return $ Return $ AVar name)
  EVar x -> do
    a <- Lookup x
    return $ Return a
  ELam formal body -> do
    let bodyName = fmap (\x -> Var $ show x ++"-body") mx
    name <- fresh mx
    body <- ModEnv (Map.insert formal (AVar formal)) $ Reset (codifyAs bodyName body)
    Wrap (LetLam name (formal,body)) (return $ Return $ AVar name)
  EApp func arg -> do
    aFunc <- atomize $ Reset (codify func) -- why reset?
    --aArg <- atomize $ Reset (codify arg)
    aArg <- atomize $ codify arg
    return $ Tail aFunc aArg
  ELet x rhs body -> do
    a <- atomizeAs (Just x) $ codifyAs (Just x) rhs
    ModEnv (Map.insert x a) $ codifyAs mx body
  where
    codify = codifyAs Nothing
    atomize = atomizeAs Nothing

atomizeAs :: Maybe Var -> M Code -> M Atom
atomizeAs mx m = do
  m >>= \case
    Return a -> return a -- dont re-name at atom
    rhs -> do
      x <- fresh mx
      Wrap (letCode' x rhs) $ return $ AVar x

-- | Avoid pushing a continutaion which calls a known function
letCode' :: Var -> Code -> Code -> Code
letCode' x rhs body
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

runM :: Env -> CompileEnv -> M Code -> Code
runM consoleEnv env m = snd $ loop env 1 m k0 where
  k0 state code = (state,code)
  loop :: CompileEnv -> State -> M a -> (State -> a -> Res) -> Res
  loop env state m k = case m of

    Ret x -> k state x
    Bind m f -> loop env state m $ \state a -> loop env state (f a) k
    Reset m -> let (state',code) = loop env state m k0 in k state' code
    Wrap f m -> f' (loop env state m k) where f' (s,a) = (s,f a)
    Lookup x -> k state (lookup x env)
    ModEnv f m -> loop (f env) state m k
    Fresh -> k (state+1) (Var ("_v" <> show state))

  lookup :: Var -> CompileEnv -> Atom
  lookup x env =
    case Map.lookup x env of
      Just atom -> atom
      Nothing ->
        case Map.lookup x consoleEnv of
          Just v -> ACon v
          Nothing ->
            error $ "Trans_Ast2Anf.lookup:"<> show x

type Res = (State,Code)
type State = Int
type CompileEnv = Map Var Atom
