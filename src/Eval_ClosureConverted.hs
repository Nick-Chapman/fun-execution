
-- | Machine to execute the closure-converted-code

module Eval_ClosureConverted (execute,Value,Instrumentation) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map

import Rep_ClosureConverted (Loc(..),Atom(..),Code(..),Value(..))
import qualified Builtin

type Result = (Value,Instrumentation)
type Instrumentation = Counts

type Machine {-m-} = (Counts,Control,Frame,Kont)
data Counts  {-i-} = Counts (Map Micro Int)
type Control {-c-} = Code
data Frame   {-f-} = Frame { fvs :: [Value], args :: [Value] }
data Kont    {-k-} = Kdone | Kbind { fvs :: [Value], code :: Code, kont :: Kont }

execute :: Code -> Result
execute = run. install

install :: Code -> Machine
install code = (counts0,code,frame0,Kdone) where frame0 = Frame [] []

run :: Machine -> Result
run (i,code0,f,k) = case code0 of

  Return atom ->
    ret i (atomic f atom) k

  Tail func args ->
    enter i (atomic f func) (map (atomic f) args) k

  LetContinue{freeFollow,rhs,follow} ->
    run (tick (saves++[DoPushContinuation]) i, rhs,f,k')
    where
      saves = map (\_ -> DoSaveFree) freeFollow
      k' = Kbind {fvs = map (locate f) freeFollow, code = follow, kont=k}

  LetPrim1 prim a1 code ->
    run (tick [DoPrim1 prim] i, code, f', k)
    where
      f' = push (doPrim1 prim (atomic f a1)) f

  LetPrim2 prim (a1,a2) code ->
    run (tick [DoPrim2 prim] i, code, f', k)
    where
      f' = push (doPrim2 prim (atomic f a1) (atomic f a2)) f

  LetClose {freeBody,arity,body,code} ->
    run (tick (saves++[DoMakeClosure]) i, code, f', k)
    where
      saves = map (\_ -> DoSaveFree) freeBody
      f' = push clo f
      clo = Clo {fvs = map (locate f') freeBody, arity, body}

  Branch a1 c2 c3 ->
    run (tick [DoBranch] i, branch c2 c3 (atomic f a1), f, k)

ret :: Counts -> Value -> Kont -> Result
ret i v = \case
  Kdone -> (v, i)
  Kbind {fvs,code,kont} -> run (i, code, Frame {fvs, args = [v]}, kont)

enter :: Counts -> Value -> [Value] -> Kont -> Result
enter i func args k = case func of
  Base{} -> error "cant enter a non-closure"
  clo@Clo{fvs,arity,body}
    | arity == n -> do
        run (tick [DoEnter] i, body, Frame {fvs,args}, k)
    | arity < n -> do
        let (myArgs,overArgs) = splitAt arity args
        let k' = makeOverAppK overArgs k
        run (tick [DoPushOverApp, DoEnter] i, body, Frame {fvs,args = myArgs}, k')
    | otherwise -> do
        ret (tick [DoMakePap] i) (makePap nMissing clo args) k
    where
      nMissing = arity - n
      n = length args

branch :: Code -> Code -> Value -> Code
branch c2 c3 = \case
  Base (Builtin.Bool True) -> c2
  Base (Builtin.Bool False) -> c3
  _ -> error "cant branch on a non boolean"

atomic :: Frame -> Atom -> Value
atomic f = \case
  ACon bv -> Base bv
  ALoc loc -> locate f loc

locate :: Frame -> Loc -> Value
locate Frame{fvs,args} = \case
  LocFree n -> nth "LocFree" fvs n
  LocArg n -> nth "LocArg" args n

push :: Value -> Frame -> Frame
push v f@Frame{args} = f { args = args ++ [v] }

doPrim1 :: Builtin.Prim1 -> Value -> Value
doPrim1 prim = \case
  Clo{} -> error $ "cant apply primitive to arg1-closure: " <> show prim
  Base bv1 -> Base (returnOrError $ Builtin.apply1 prim bv1)

doPrim2 :: Builtin.Prim2 -> Value -> Value -> Value
doPrim2 prim = \case
  Clo{} -> \_ -> error $ "cant apply primitive to arg1-closure: " <> show prim
  Base bv1 -> \case
    Clo{} -> error $ "cant apply primitive to arg2-closure: " <> show (prim,bv1)
    Base bv2 -> Base (returnOrError $ Builtin.apply2 prim (bv1,bv2))

returnOrError :: Show e => Either e a -> a
returnOrError = \case
  Left e -> error $ show e
  Right a -> a

makeOverAppK :: [Value] -> Kont -> Kont
makeOverAppK overArgs kont = Kbind {fvs=overArgs, code, kont}
  where
    code = Tail (ALoc (LocArg 0)) args
    args = [ ALoc (LocFree i) | i <- [0 .. length overArgs - 1] ]

makePap :: Int -> Value -> [Value] -> Value
makePap nMissing clo argsSoFar = clo2
  where
    clo2 = Clo {fvs = clo : argsSoFar, arity = nMissing, body}
    body = Tail (ALoc (LocFree 0)) args
    args =
      [ ALoc (LocFree i) | i <- [1 .. length argsSoFar] ] ++
      [ ALoc (LocArg i) | i <- [0 .. nMissing - 1] ]

nth :: Show a => String -> [a] -> Int -> a
nth tag xs i = if i >= length xs then error (show (tag,i,xs)) else xs !! i

----------------------------------------------------------------------
-- instrumentation

tick :: [Micro] -> Counts -> Counts
tick mics i = foldl countMicro i mics

instance Show Counts where
  show (Counts m) =
    unlines $ map (\(c,i) -> unwords ["-", show c, show i]) $ Map.toList m

counts0 :: Counts
counts0 = Counts Map.empty

countMicro :: Counts -> Micro -> Counts
countMicro (Counts mm) cl = Counts (Map.insertWith (+) cl 1 mm)

data Micro
  = DoEnter
  | DoPushContinuation
  | DoPushOverApp
  | DoPrim1 Builtin.Prim1
  | DoPrim2 Builtin.Prim2
  | DoMakeClosure
  | DoMakePap
  | DoBranch
  | DoSaveFree
  deriving (Show,Eq,Ord)
