
module Pipeline3 (CompilationError,Code,Value,Instrumentation,compile,execute) where

-- Pipeline: Ast -> Anf -> ClosureConverted

import Rep_Ast (Exp)
import Rep_ClosureConverted (Code)
import Eval_ClosureConverted (Value,Instrumentation)
import CheckClosed_Ast (checkClosed)
import Trans_Ast2Anf (flatten)
import Trans_Anf2CC (convert)
import Eval_ClosureConverted (execute)

data CompilationError = CompilationError { unCompilationError :: String }
instance Show CompilationError where show = unCompilationError

compile :: Exp -> Either CompilationError Code
compile exp =
  case checkClosed exp of
    Just err -> Left $ CompilationError $ show err
    Nothing -> Right $ convert $ flatten exp
