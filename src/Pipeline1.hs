
module Pipeline1 (CompilationError,Code,Value,Instrumentation,check,compile,execute) where

import qualified System.Console.ANSI as AN

-- trivial pipeline: Ast is Code; compilation is just to check for unbound identifiers

import CheckClosed_Ast (checkClosed)
import Eval_Ast (Value)
import Rep_Ast (Exp,indented,pretty)
import qualified Eval_Ast as Ast (evaluate)

import Trans_Normalize (normalize)

data Code = Code Exp
instance Show Code where show (Code e) = unlines $ indented "CODE:" (pretty e)

data CompilationError = CompilationError { unCompilationError :: String }
instance Show CompilationError where show = unCompilationError

check :: Exp -> Maybe CompilationError
check exp = (CompilationError . show) <$> checkClosed exp

compile :: Exp -> IO (Either CompilationError Code)
compile exp =
  case checkClosed exp of
    Just err -> return $ Left $ CompilationError $ show err
    Nothing -> do
      putStr $ col AN.Yellow (show exp)
      exp' <- pure $ normalize exp
      putStr $ col AN.Green (show exp')
      return $ Right (Code exp')

execute :: Code -> (Value,Instrumentation)
execute (Code exp) = (Ast.evaluate exp, ())

type Instrumentation = ()

col :: AN.Color -> String -> String
col c s =
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid c] <> s <>
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid AN.White]
