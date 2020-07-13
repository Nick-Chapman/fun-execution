
-- | Convert Anf to CC (Closure converted) Code

module Trans_Anf2CC (convert) where

import Control.Monad(ap,liftM)
import Data.Map (Map)
import Data.Set (Set,(\\))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Rep_Anf (Var(..))
import Rep_ClosureConverted (Loc(..),Atom(..),Code(..))
import qualified Config
import qualified Rep_Anf as Anf

convert :: Anf.Code -> Code
convert = runM [] . convertAnf

convertAnf :: Anf.Code -> M Code
convertAnf = \case
  Anf.Return a -> Return <$> convertAtom a

  Anf.Tail func args -> do
    func <- convertAtom func
    args <- mapM convertAtom args
    return $ Tail func args

  Anf.LetPrim1 x prim a1 code -> do
    a1 <- convertAtom a1
    code <- Extend [x] $ convertAnf code
    return $ LetPrim1 prim a1 code

  Anf.LetPrim2 x prim (a1,a2) code -> do
    a1 <- convertAtom a1
    a2 <- convertAtom a2
    code <- Extend [x] $ convertAnf code
    return $ LetPrim2 prim (a1,a2) code

  Anf.LetLam y (xs,body0) code -> do
    let arity = length xs
    let fvs = freeVarList (xs,body0)
    let body = runM fvs (Extend xs $ convertAnf body0)
    freeBody <- mapM Lookup fvs
    code <- Extend [y] $ convertAnf code
    return $ LetClose {freeBody,arity,body,code}

  Anf.LetFix f (xs,body0) code -> do
    let arity = length xs
    let fvs = freeVarList (xs,body0)
    let body = runM fvs (Extend xs $ convertAnf body0)
    freeBody <- Extend [f] $ mapM Lookup fvs
    code <- Extend [f] $ convertAnf code
    return $ LetClose {freeBody,arity,body,code}

  Anf.LetCode y rhs follow0 -> do
    let fvs = freeVarList ([y],follow0)
    let follow =
          case Config.fvsOnStack of
            True  -> runM [] (Extend (fvs++[y]) $ convertAnf follow0)
            False -> runM fvs (Extend [y] $ convertAnf follow0)
    freeFollow <- mapM Lookup fvs
    rhs <- convertAnf rhs
    return $ LetContinue {freeFollow,rhs,follow}

  Anf.Branch a1 c2 c3 -> do
    a1 <- convertAtom a1
    c2 <- convertAnf c2
    c3 <- convertAnf c3
    return $ Branch a1 c2 c3

convertAtom :: Anf.Atom -> M Atom
convertAtom = \case
  Anf.ACon bv -> return $ ACon bv
  Anf.AVar x -> ALoc <$> Lookup x

freeVarList :: ([Var],Anf.Code) -> [Var]
freeVarList (bound,code) = Set.toList $ fvsBinding (bound,code)

fvsBinding :: ([Var],Anf.Code) -> Set Var
fvsBinding (vars,code) = fvsCode code \\ Set.fromList vars

fvsCode :: Anf.Code -> Set Var
fvsCode = \case
  Anf.Return a -> fvsAtom a
  Anf.Tail func args -> fvsAtom func <> Set.unions (map fvsAtom args)
  Anf.LetCode x rhs follow -> fvsCode rhs <> fvsBinding ([x],follow)
  Anf.LetPrim1 x _ a1 code -> fvsAtom a1 <> fvsBinding ([x],code)
  Anf.LetPrim2 x _ (a1,a2) code -> fvsAtom a1 <> fvsAtom a2 <> fvsBinding ([x],code)
  Anf.LetLam y (xs,body) code -> fvsBinding (xs,body) <> fvsBinding ([y],code)
  Anf.LetFix f (xs,body) code -> fvsBinding (f:xs,body) <> fvsBinding ([f],code)
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
  Lookup :: Var -> M Loc
  Extend :: [Var] -> M a -> M a

runM :: [Var] -> M a -> a
runM fvs = loop 0 env0 where
  env0 = Map.fromList [ (v,LocFree i) | (v,i) <- zip fvs [0..] ]
  loop :: Int -> Env -> M a -> a
  loop d env = \case
    Ret x -> x
    Bind m f -> loop d env (f (loop d env m))
    Lookup x -> maybe (error $ "lookup: " ++ show x) id $ Map.lookup x env
    Extend xs m -> do
      let env' = Map.fromList [ (x,LocArg i) | (x,i) <- zip xs [d..] ]
      loop (d + length xs) (Map.union env' env) m

type Env = Map Var Loc
