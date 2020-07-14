
module Eval_Lin (execute,Value,Instrumentation) where

import Builtin (CommandLineArgs,BV(..),Prim1,Prim2,apply1,apply2)
import Control.Monad (when)
import Data.Maybe (fromJust)
import Rep4_Lin (Code(..),CodeSequence(..),CodeRef(..),LitRef(..),Index(..),ValRef(..))
import RuntimeCallingConventions (RT(..),ContFreeVars(..))

trace :: Bool
trace = False

data Value
  = Base Builtin.BV
  | Clo { fvs :: [Value], body :: CodeSequence }

instance Show Value where
  show = \case
    Base bv -> show bv
    Clo{} -> "<closure>"

type Result = (Value,Instrumentation)
data Instrumentation = NoInstrumentation deriving Show

data Machine  = Machine
  { fvs :: [Value]
  , stack :: [Value]
  , kont :: Kont
  , this_closure :: Maybe Value
  }

data Kont = Kdone | Kbind { fvs :: [Value], code :: CodeSequence, next :: Kont }

execute :: CommandLineArgs -> Code -> IO Result
execute cla Code{lits,defs,rt} = do v <- run0 machine0 seq0; return (v,NoInstrumentation)
  where
    machine0 = Machine { stack = [], fvs = [], kont = Kdone, this_closure = Nothing }
    seq0 = getSeq (CodeRef (Index 0))

    getSeq :: CodeRef -> CodeSequence
    getSeq (CodeRef (Index i)) = nth "defs" defs i

    run0 :: Machine -> CodeSequence -> IO Value
    run0 m seq = do
      when trace $ putStrLn $ "set_code: " <> show seq
      run m seq

    run :: Machine -> CodeSequence -> IO Value
    run m@Machine{stack,kont} = \case

      UnconditionalJump r -> run0 m (getSeq r)

      ArityCheck (Index arity) c
        | got == arity -> run m c
        | got < arity -> returnToContinuation (makePap nMissing clo stack)
        | otherwise -> do
            let (myArgs,overArgs) = splitAt arity stack
            run m { stack = myArgs, kont = makeOverAppK rt overArgs kont } c
       where
         nMissing = arity - got
         got = length stack
         clo = let Machine{this_closure} = m in fromJust this_closure

      Return v -> returnToContinuation (locate m v)

      Tail func args -> enterClosure (locate m func) (map (locate m) args)

      Prim1 p a c -> do
        v <- doPrim1 cla p (locate m a)
        run (pushStack v) c

      Prim2 p a1 a2 c -> do
        v <- doPrim2 p (locate m a1) (locate m a2)
        run (pushStack v) c

      JumpIfTrue r v c -> do
        testCondition (locate m v) >>= \case
          True -> run0 m (getSeq r)
          False -> run m c

      PushContinuation r vs c ->
        run m { kont = Kbind{fvs = map (locate m) vs,code = getSeq r, next = kont} } c

      MakeClosure r vs c -> run m' c where
        m' = pushStack v
        v = Clo { fvs = map (locate m') vs, body = getSeq r } -- use of m' create self-ref

     where
       returnToContinuation :: Value -> IO Value
       returnToContinuation v = case kont of
         Kdone -> return v
         Kbind{fvs,code,next} ->
           case contFreeVars rt of
             FOS ->
               -- Dump free vars onto stack
               run0 m {stack = fvs ++ [v], fvs = [], kont = next} code
             FIF ->
               -- Leave free vars in frame
               run0 m {stack = [v], fvs, kont = next} code

       enterClosure :: Value -> [Value] -> IO Value
       enterClosure func args = do
         case func of
          Base bv -> fail $ "cant enter a non-closure: " ++ show bv
          Clo{fvs,body} ->
            run0 m {stack = args, fvs, this_closure = Just func} body

       pushStack :: Value -> Machine
       pushStack v = m { stack = stack ++ [v] }

    locate :: Machine -> ValRef -> Value
    locate Machine{stack,fvs} = \case
      VArg (Index i) -> nth "stack" stack i
      VFree (Index i) -> nth "fvs" fvs i
      VLit (LitRef (Index i))  -> Base (nth "lits" lits i)

makePap :: Int -> Value -> [Value] -> Value
makePap nMissing clo argsSoFar = clo2
  where
    clo2 = Clo {fvs = clo : argsSoFar, body}
    body = ArityCheck (Index nMissing) $ Tail (VFree (Index 0)) args
    args =
      [ VFree (Index i) | i <- [1 .. length argsSoFar] ] ++
      [ VArg (Index i) | i <- [0 .. nMissing - 1] ]

makeOverAppK :: RT -> [Value] -> Kont -> Kont
makeOverAppK rt overArgs kont = Kbind {fvs=overArgs, code, next = kont}
  where
    n = length overArgs
    code = case contFreeVars rt of
      FOS ->
        -- When a continuation is entered, free vars are pushed to the stack:
        Tail (VArg (Index n)) [ VArg (Index i) | i <- [0 .. n-1] ]
      FIF ->
        -- When a continuation is entered, free vars are found in the frame.
        Tail (VArg (Index 0)) [ VFree (Index i) | i <- [0 .. n-1] ]

testCondition :: Value -> IO Bool
testCondition = \case
  Base (Builtin.Bool b) -> return b
  _ -> fail "cant branch on a non boolean"

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

nth :: Show a => String -> [a] -> Int -> a
nth tag xs i = if i >= length xs then error (show (tag,i,xs)) else xs !! i
