
-- Linearized code seqeunces

module Trans_CC2Linear(linearize) where

import Control.Monad (ap,liftM)
import qualified Builtin
import qualified Rep_ClosureConverted as CC

-- module Rep_Linearlized...
----------------------------------------------------------------------
data CodeSequence
  = UnconditionalJump CodeRef
  | Return ValRef
  | Tail ValRef [ValRef]
  | Prim1 Builtin.Prim1 ValRef CodeSequence
  | Prim2 Builtin.Prim2 ValRef ValRef CodeSequence
  | JumpIfTrue CodeRef ValRef CodeSequence
  | PushContinuation CodeRef [ValRef] CodeSequence
  | MakeClosure (Int,CodeRef) [ValRef] CodeSequence

data ValRef = VArg Int | VFree Int | VLit Int

newtype CodeRef = CodeRef Int

data Code = Code
  { literalTable :: [Builtin.BV]
  , codeTable :: [CodeSequence]
  }

----------------------------------------------------------------------

instance Show Code where
  show Code{literalTable,codeTable} =
    unlines
    ( ("lits = " ++ show literalTable)
      : [ show i ++ ": " ++ show seq | (i,seq) <- zip [0::Int ..] codeTable ] )

instance Show ValRef where
  show = \case
    VArg n -> "a" ++ showInt n
    VFree n -> "f" ++ showInt n
    VLit n -> "l" ++ showInt n

instance Show CodeRef where
  show = \case
    CodeRef n -> showInt n

instance Show CodeSequence where
  show = \case
    UnconditionalJump r -> "u" ++ show r
    Return v -> "r" ++ show v
    Tail func args -> "t" ++ show func ++ showVals args
    Prim1 p a c -> showPrim1 p ++ show a ++ show c
    Prim2 p a1 a2 c -> showPrim2 p ++ show a1 ++ show a2 ++ show c
    JumpIfTrue r v c -> "j" ++ show v ++ show r ++ show c
    PushContinuation r vs c -> "p" ++ show r ++ showVals vs ++ show c
    MakeClosure (arity,r) vs c -> "c" ++ show r ++ showInt arity ++ showVals vs ++ show c

showVals :: [ValRef] -> String
showVals vs = showInt (length vs) ++ concat (map show vs)

showInt :: Int -> String
showInt n = do
  take (length ds - 1) (repeat 'x') ++ concat (map showDigit ds)
  where
    ds = digits [] n
    digits acc n = if n<10 then (n:acc) else
      digits ((n `mod` 10) : acc) (n `div` 10)

showDigit :: Int -> String
showDigit n = if n<0 || n>9 then error "showDigit" else show n

showPrim1 :: Builtin.Prim1 -> String
showPrim1 = \case
  Builtin.ShowInt -> "&"

showPrim2 :: Builtin.Prim2 -> String
showPrim2 = \case
  Builtin.Add -> "+"
  Builtin.Sub -> "-"
  Builtin.Mul -> "*"
  Builtin.ModInt -> "%"
  Builtin.EqInt -> "="
  Builtin.LessInt -> "<"
  Builtin.StringAppend -> "^"

----------------------------------------------------------------------

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
    body <- walk body >>= CutCode
    code <- walk code
    return $ MakeClosure (arity,body) freeBody code

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
  CC.LocArg n -> VArg n
  CC.LocFree n -> VFree n

----------------------------------------------------------------------

instance Functor M where fmap = liftM
instance Applicative M where pure = return; (<*>) = ap
instance Monad M where return = Ret; (>>=) = Bind

data M a where
  Ret :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  Literal :: Builtin.BV -> M Int
  CutCode :: CodeSequence -> M CodeRef

data State = State
  { lits :: [Builtin.BV] , litN :: Int
  , defs :: [CodeSequence] , defN :: Int
  }

runM :: M CodeRef -> Code
runM m = finalCode where

  finalCode = Code { literalTable = reverse lits, codeTable = def0 : reverse defs }

  def0 = UnconditionalJump startRef
  (startRef, State{lits,defs}) = loop state0 m

  state0 = State { lits = [], defs = [], litN = 0, defN = 1 }

  loop :: State -> M a -> (a,State)
  loop s = \case
    Ret x -> (x,s)
    Bind m f -> let (v,s') = loop s m in loop s' (f v)
    CutCode def -> do
      let State{defs,defN=n} = s
      let s' = s { defN = n + 1, defs = def : defs }
      (CodeRef n, s')
    Literal lit -> do
      let State{lits,litN=n} = s
      let s' = s { litN = n + 1, lits = lit : lits }
      (n, s')
