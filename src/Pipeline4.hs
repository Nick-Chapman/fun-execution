
-- Pipeline: Ast -> Anf -> ClosureConverted -> Linear

module Pipeline4 (
  Opt(..),
  check, CompilationError,
  compile, quietCompile, Code,
  execute, Value, Instrumentation,
  ) where

import Control.Exception(try)
import qualified System.Console.ANSI as AN

import Builtin (CommandLineArgs)
import Rep_Ast (Exp)
import Rep_Linear (Code)
import Eval_Linear (Value,Instrumentation)
import CheckClosed_Ast (checkClosed)
import Trans_Normalize (normalize)
import Trans_Ast2Anf (flatten)
import Trans_Anf2CC (convert)
import Trans_CC2Linear (linearize)
import qualified Eval_Linear as Eval (execute)

data Opt = NoOpt | NbE

data CompilationError = CompilationError { unCompilationError :: String }
instance Show CompilationError where show = unCompilationError

check :: Exp -> Maybe CompilationError
check exp = (CompilationError . show) <$> checkClosed exp

put :: String -> IO ()
put = if optPut then putStr else \_ -> return ()
  where optPut = False  -- TODO: select via opt

quietCompile :: Opt -> Exp -> IO Code
quietCompile opt exp = do
  put $ col AN.Yellow (show exp)
  exp' <-
    case opt of
      NoOpt -> pure exp
      NbE -> do
        let exp' = normalize exp
        put $ col AN.Green (show exp')
        pure exp'
  let anf = flatten exp'
  put $ col AN.Blue (show anf)
  let cc = convert anf
  put $ col AN.Magenta (show cc)
  let lin = linearize cc
  put $ col AN.White (show lin)
  return lin

col :: AN.Color -> String -> String
col c s =
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid c] <> s <>
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid AN.White]

compile :: Opt -> Exp -> IO (Either CompilationError Code)
compile opt exp = do
  case checkClosed exp of
    Just err -> return $ Left $ CompilationError $ show err
    Nothing -> Right <$> quietCompile opt exp

execute :: CommandLineArgs -> Code -> IO (Either String (Value, Instrumentation))
execute cla x = do
  either (Left . show) Right <$> (try @IOError $ Eval.execute cla x)
