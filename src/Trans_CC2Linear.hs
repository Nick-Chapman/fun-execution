
module Trans_CC2Linear(linearize) where

import Control.Monad (ap,liftM)
import qualified Builtin
import qualified Rep_ClosureConverted as CC
import Rep_Linear

linearize :: CC.Code -> Code
linearize cc = runM (walk cc >>= CutCode)

walk :: CC.Code -> M CodeSequence
walk = \case

  CC.Return a -> do
    Return <$> walkA a

  CC.Tail func args -> do
    func <- walkA func
    args <- mapM walkA args
    return $ Tail func args

  CC.LetContinue{freeFollow,rhs,follow} -> do
    freeFollow <- pure $ map walkLoc freeFollow
    follow <- walk follow >>= CutCode
    rhs <- walk rhs
    return $ PushContinuation follow freeFollow rhs

  CC.LetPrim1 prim a1 code -> do
    a1 <- walkA a1
    code <- walk code
    return $ Prim1 prim a1 code

  CC.LetPrim2 prim (a1,a2) code -> do
    a1 <- walkA a1
    a2 <- walkA a2
    code <- walk code
    return $ Prim2 prim a1 a2 code

  CC.LetClose{freeBody,arity,body,code} -> do
    freeBody <- pure $ map walkLoc freeBody
    body <- (ArityCheck (Index arity) <$> walk body) >>= CutCode
    code <- walk code
    return $ MakeClosure body freeBody code

  CC.Branch a1 c2 c3 -> do
    a1 <- walkA a1
    c2 <- walk c2 >>= CutCode
    c3 <- walk c3
    return $ JumpIfTrue c2 a1 c3

walkA :: CC.Atom -> M ValRef
walkA = \case
  CC.ALoc loc -> return $ walkLoc loc
  CC.ACon bv -> VLit <$> Literal bv

walkLoc :: CC.Loc -> ValRef
walkLoc = \case
  CC.LocArg n -> VArg $ Index n
  CC.LocFree n -> VFree $ Index n

----------------------------------------------------------------------

instance Functor M where fmap = liftM
instance Applicative M where pure = return; (<*>) = ap
instance Monad M where return = Ret; (>>=) = Bind

data M a where
  Ret :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  Literal :: Builtin.BV -> M LitRef
  CutCode :: CodeSequence -> M CodeRef

type State = Code

runM :: M CodeRef -> Code
runM m = finalCode where

  def0 = UnconditionalJump startRef
  (startRef, finalCode) = loop state0 m

  state0 = Code { lits = [], defs = [def0] }

  loop :: State -> M a -> (a,State)
  loop s@Code{lits=lits0,defs=defs0} = \case
    Ret x -> (x,s)
    Bind m f -> let (v,s') = loop s m in loop s' (f v)
    CutCode def -> do
      let (n, defs) = addMaybeCaching def defs0
      (CodeRef (Index n), s { defs })
    Literal lit -> do
      let (n, lits) = searchOrAdd lit lits0
      (LitRef (Index n), s { lits })

  addMaybeCaching :: Eq a => a -> [a] -> (Int,[a])
  addMaybeCaching =
    if doCache then searchOrAdd else alwaysAdd

  alwaysAdd :: Eq a => a -> [a] -> (Int,[a])
  alwaysAdd k xs0 = (length xs0, xs0 ++ [k])

  searchOrAdd :: Eq a => a -> [a] -> (Int,[a])
  searchOrAdd k xs0 = loop 0 xs0 where
    loop i = \case
      [] -> (i, xs0 ++ [k])
      x:xs -> if x==k then (i, xs0) else loop (i+1) xs

  doCache = True
