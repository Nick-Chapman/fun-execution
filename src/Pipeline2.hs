
module Pipeline2 (CompilationError,Code,Value,Instrumentation,compile,execute) where

-- Pipeline: Ast -> Anf

import Rep_Ast (Exp)
import Rep_Anf (Code)
import CheckClosed_Ast (checkClosed)
import Trans_Ast2Anf (flatten)
import Eval_Anf (Value,evaluate)

data CompilationError = CompilationError { unCompilationError :: String }
instance Show CompilationError where show = unCompilationError

compile :: Exp -> Either CompilationError Code
compile exp =
  case checkClosed exp of
    Just err -> Left $ CompilationError $ show err
    Nothing -> Right $ flatten exp

execute :: Code -> (Value,Instrumentation)
execute code = (evaluate code, ())

type Instrumentation = ()
