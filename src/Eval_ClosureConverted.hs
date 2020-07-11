
-- | Machine to execute the closure-converted-code

module Eval_ClosureConverted (execute,Value,Instrumentation) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map

import Rep_ClosureConverted (Loc(..),Atom(..),Code(..),Value(..))
import qualified Builtin
import Builtin(CommandLineArgs)

type Result = (Value,Instrumentation)
type Instrumentation = Counts

data Machine = Machine { cla :: CommandLineArgs, i :: Counts, c :: Control, f :: Frame, k :: Kont }

data Counts  {-i-} = Counts (Map Micro Int)
type Control {-c-} = Code
data Frame   {-f-} = Frame { fvs :: [Value], args :: [Value] }
data Kont    {-k-} = Kdone | Kbind { fvs :: [Value], code :: Code, kont :: Kont }

execute :: CommandLineArgs -> Code -> IO Result
execute cla = run . install cla

install :: CommandLineArgs ->  Code -> Machine
install cla code = Machine cla counts0 code frame0 Kdone where frame0 = Frame [] []

run :: Machine -> IO Result
run Machine{cla,i,c=code0,f,k} = do
 --print "run"
 --let Frame{fvs} = f in print (length fvs)
 --print code0
 case code0 of

  Return atom ->
    ret cla i (atomic f atom) k

  Tail func args ->
    enter cla i (atomic f func) (map (atomic f) args) k

  LetContinue{freeFollow,rhs,follow} ->
    run $ Machine cla (tick (saves++[DoPushContinuation]) i) rhs f k'
    where
      saves = map (\_ -> DoSaveFree) freeFollow
      k' = Kbind {fvs = map (locate f) freeFollow, code = follow, kont=k}

  LetPrim1 prim a1 code -> do
    v <- doPrim1 cla prim (atomic f a1)
    run $ Machine cla (tick [DoPrim1 prim] i) code (push v f) k

  LetPrim2 prim (a1,a2) code -> do
    v <- doPrim2 prim (atomic f a1) (atomic f a2)
    run $ Machine cla (tick [DoPrim2 prim] i) code (push v f) k

  LetClose {freeBody,arity,body,code} ->
    run $ Machine cla (tick (saves++[DoMakeClosure]) i) code f' k
    where
      saves = map (\_ -> DoSaveFree) freeBody
      f' = push clo f
      clo = Clo {fvs = map (locate f') freeBody, arity, body}

  Branch a1 c2 c3 -> do
    c <- branch c2 c3 (atomic f a1)
    run $ Machine cla (tick [DoBranch] i) c f k

ret :: CommandLineArgs -> Counts -> Value -> Kont -> IO Result
ret cla i v = \case
  Kdone -> return (v, i)
  Kbind {fvs,code,kont} -> run $ Machine cla i code Frame {fvs = [], args = fvs++[v]} kont

enter :: CommandLineArgs -> Counts -> Value -> [Value] -> Kont -> IO Result
enter cla i func args k = do
 --print "enter"
 --print (func,args)
 case func of
  Base bv -> fail $ "cant enter a non-closure: " ++ show bv
  clo@Clo{fvs,arity,body}
    | arity == got -> do
        run $ Machine cla (tick [DoEnter] i) body Frame {fvs,args} k
    | arity < got -> do
        let (myArgs,overArgs) = splitAt arity args
        let k' = makeOverAppK overArgs k
        run $ Machine cla (tick [DoPushOverApp (length overArgs), DoEnter] i) body Frame {fvs,args = myArgs} k'
    | otherwise -> do
        ret cla (tick [DoMakePap {got,need=arity}] i) (makePap nMissing clo args) k
    where
      nMissing = arity - got
      got = length args

branch :: Code -> Code -> Value -> IO Code
branch c2 c3 = \case
  Base (Builtin.Bool True) -> return c2
  Base (Builtin.Bool False) -> return c3
  _ -> fail "cant branch on a non boolean"

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

doPrim1 :: CommandLineArgs -> Builtin.Prim1 -> Value -> IO Value
doPrim1 cla prim = \case
  Clo{} -> fail $ "cant apply primitive to arg1-closure: " <> show prim
  Base bv1 -> Base <$> (returnOrFail $ Builtin.apply1 cla prim bv1)

doPrim2 :: Builtin.Prim2 -> Value -> Value -> IO Value
doPrim2 prim = \case
  Clo{} -> \_ -> fail $ "cant apply primitive to arg1-closure: " <> show prim
  Base bv1 -> \case
    Clo{} -> fail $ "cant apply primitive to arg2-closure: " <> show (prim,bv1)
    Base bv2 -> Base <$> (returnOrFail $ Builtin.apply2 prim (bv1,bv2))

returnOrFail :: Show e => Either e a -> IO a
returnOrFail = \case
  Left e -> fail $ show e
  Right a -> return a

makeOverAppK :: [Value] -> Kont -> Kont
makeOverAppK overArgs kont = Kbind {fvs=overArgs, code, kont}
  where
-- OLD: When a continuation is entered, free vars are found in the frame.
{-
    code = Tail (ALoc (LocArg 0)) args
    args = [ ALoc (LocFree i) | i <- [0 .. n-1] ]
-}
-- When a continuation is entered, free vars are pushed to the stack:
    code = Tail (ALoc (LocArg n)) args
    args = [ ALoc (LocArg i) | i <- [0 .. n-1] ]
    n = length overArgs

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
  | DoPushOverApp {overArgs::Int}
  | DoPrim1 Builtin.Prim1
  | DoPrim2 Builtin.Prim2
  | DoMakeClosure
  | DoMakePap {got::Int,need::Int}
  | DoBranch
  | DoSaveFree
  deriving (Show,Eq,Ord)
