
module Pipeline1 (CompilationError,Code,Value,Instrumentation,compile,execute) where

-- trivial pipeline: Ast is Code; compilation is just to check for unbound identifiers

import CheckClosed_Ast (checkClosed)
import Eval_Ast (Value)
import Rep_Ast (Exp,indented,pretty)
import qualified Eval_Ast as Ast (evaluate)

data Code = Code Exp
instance Show Code where show (Code e) = unlines $ indented "CODE:" (pretty e)

data CompilationError = CompilationError { unCompilationError :: String }
instance Show CompilationError where show = unCompilationError

compile :: Exp -> Either CompilationError Code
compile exp =
  case checkClosed exp of
    Just err -> Left $ CompilationError $ show err
    Nothing -> Right (Code exp)

execute :: Code -> (Value,Instrumentation)
execute (Code exp) = (Ast.evaluate exp, ())

type Instrumentation = ()
