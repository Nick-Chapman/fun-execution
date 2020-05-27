
module Trans_Anf2CC (convert,Env) where

import Control.Monad(ap,liftM)
import Data.Map (Map)
import Data.Set (Set,(\\))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Rep_Anf (Var(..))
import Rep_ClosureConverted (Loc(..),Atom(..),Code(..),Value(..))
import qualified Rep_Anf as Anf

type Env = Map Anf.Var Value

----------------------------------------------------------------------
-- convert Anf to (Closure converted) Code

convert :: Env -> Anf.Code -> Code
convert env anf = runM env (convertAnf anf) -- TODO: use Env

convertAnf :: Anf.Code -> M Code
convertAnf = \case
  Anf.Return a -> Return <$> convertAtom a

  Anf.Tail func arg -> do -- TODO: multi-Anf
    func <- convertAtom func
    args <- mapM convertAtom [arg]
    return $ Tail func args

  Anf.LetOp x op (a1,a2) code -> do
    a1 <- convertAtom a1
    a2 <- convertAtom a2
    code <- Extend [x] $ convertAnf code
    return $ LetOp op (a1,a2) code

  Anf.LetLam y (x,body) code -> do
    let xs = [x]
    fvs <- freeVarList (xs,body)
    let arity = length xs
    let locations = [ (v,LocFree i) | (v,i) <- zip fvs [0..] ]
    freeBody <- Extend [Var "_self"] $ mapM Lookup fvs
    body <- Reset locations $ Extend xs $ convertAnf body
    code <- Extend [y] $ convertAnf code
    return $ LetClose {freeBody,arity,body,code}

{-
  Anf.LetFix f (xs,body) code -> do
    fvs <- freeVarList (xs,body)
    let arity = length xs
    let locations = [ (v,LocFree i) | (v,i) <- zip fvs [0..] ]
    freeBody <- Extend [f] $ mapM Lookup fvs
    body <- Reset locations $ Extend xs $ convertAnf body
    code <- Extend [f] $ convertAnf code
    return $ LetClose {freeBody,arity,body,code}
-}
  Anf.Branch a1 c2 c3 -> do
    a1 <- convertAtom a1
    c2 <- convertAnf c2
    c3 <- convertAnf c3
    return $ Branch a1 c2 c3

  Anf.LetCode y rhs follow -> do
    fvs <- freeVarList ([y],follow)
    let locations = [ (v,LocFree i) | (v,i) <- zip fvs [0..] ]
    freeFollow <- mapM Lookup fvs
    rhs <- convertAnf rhs
    follow <- Reset locations $ Extend [y] $ convertAnf follow
    return $ LetContinue {freeFollow,rhs,follow}

convertAtom :: Anf.Atom -> M Atom
convertAtom = \case
  Anf.ACon bv -> return $ ACon $ Base bv
  Anf.AVar x -> do
    topEnv <- TopEnv
    case Map.lookup x topEnv of
      Just v -> return $ ACon v
      Nothing -> ALoc <$> Lookup x

freeVarList :: ([Var],Anf.Code) -> M [Var]
freeVarList (bound,code) = do
  bound2 <- Map.keys <$> TopEnv
  return $ Set.toList $ fvsBinding (bound++bound2,code)

fvsBinding :: ([Var],Anf.Code) -> Set Var
fvsBinding (vars,code) = fvsCode code \\ Set.fromList vars

fvsCode :: Anf.Code -> Set Var
fvsCode = \case
  Anf.Return a -> fvsAtom a
  Anf.Tail func arg -> fvsAtom func <> Set.unions (map fvsAtom [arg])
  Anf.LetCode x rhs follow -> fvsCode rhs <> fvsBinding ([x],follow)
  Anf.LetOp x _ (a1,a2) code -> fvsAtom a1 <> fvsAtom a2 <> fvsBinding ([x],code)
  Anf.LetLam y (x,body) code -> fvsBinding ([x],body) <> fvsBinding ([y],code)
--  Anf.LetFix f (xs,body) code -> fvsBinding (f:xs,body) <> fvsBinding ([f],code)
  Anf.Branch a1 c2 c3 -> fvsAtom a1 <> fvsCode c2 <> fvsCode c3

fvsAtom :: Anf.Atom -> Set Var
fvsAtom = \case
  Anf.ACon{} -> Set.empty
  Anf.AVar x -> Set.singleton x


instance Functor M where fmap = liftM
instance Applicative M where pure = return; (<*>) = ap
instance Monad M where return = Ret; (>>=) = Bind

data M a where
  Ret :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  TopEnv :: M (Map Var Value)
  Lookup :: Var -> M Loc
  Extend :: [Var] -> M a -> M a
  Reset :: [(Var,Loc)] -> M a -> M a

runM :: Env -> M a -> a
runM topEnv = loop 0 Map.empty where
  loop :: Int -> LocEnv -> M a -> a
  loop d env = \case
    Ret x -> x
    Bind m f -> loop d env (f (loop d env m))
    Reset env m -> loop 0 (Map.fromList env) m
    TopEnv -> topEnv
    Lookup x ->
      case Map.lookup x env of
        Just loc -> rel d loc
        Nothing -> error $ "compile-time-lookup: " ++ show (x,env)
    Extend xs m -> do
      let env' = Map.fromList [ (x,LocArg i) | (x,i) <- zip (reverse xs) [d..] ]
      loop (d + length xs) (Map.union env' env) m

rel :: Int -> Loc -> Loc -- relativize a location to a stack depth
rel d = \case
  loc@LocFree{} -> loc
  LocArg n -> LocArg (d-n-1) -- TODO: could this be avoided with absolute offsets?

type LocEnv = Map Var Loc
