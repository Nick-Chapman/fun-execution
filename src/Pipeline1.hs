
module Pipeline1 (CompilationError,Code,Value,Instrumentation,check,compile,execute,Opt(..)) where

import qualified System.Console.ANSI as AN

-- trivial pipeline: Ast is Code; compilation is just to check for unbound identifiers

import Builtin(CommandLineArgs)
import CheckClosed_Ast (checkClosed)
import Eval_Ast (Value)
import Rep_Ast (Exp,indented,pretty)
import Trans_Normalize (normalize)
import qualified Eval_Ast as Ast (evaluate)

data Opt = NoOpt | NbE

data Code = Code Exp
instance Show Code where show (Code e) = unlines $ indented "CODE:" (pretty e)

data CompilationError = CompilationError { unCompilationError :: String }
instance Show CompilationError where show = unCompilationError

check :: Exp -> Maybe CompilationError
check exp = (CompilationError . show) <$> checkClosed exp

compile :: Opt -> Exp -> IO (Either CompilationError Code)
compile opt exp =
  case checkClosed exp of
    Just err -> return $ Left $ CompilationError $ show err
    Nothing -> do
      putStr $ col AN.Yellow (show exp)
      exp' <-
        case opt of
          NoOpt -> pure exp
          NbE -> do
            let exp' = normalize exp
            putStr $ col AN.Green (show exp')
            pure exp'
      return $ Right (Code exp')

execute :: CommandLineArgs -> Code -> IO (Value,Instrumentation)
execute _cla_IGNORED (Code exp) = return(Ast.evaluate exp, ())

type Instrumentation = ()

col :: AN.Color -> String -> String
col c s =
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid c] <> s <>
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid AN.White]
