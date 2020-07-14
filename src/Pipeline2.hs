
module Pipeline2 (CompilationError,Code,Value,Instrumentation,check,compile,execute) where

-- Pipeline: Ast -> Anf

import Rep1_Ast (Exp)
import Rep2_Anf (Code)
import CheckClosed_Ast (checkClosed)
import Trans12_Ast2Anf (flatten)
import Eval_Anf (Value,evaluate)

data CompilationError = CompilationError { unCompilationError :: String }
instance Show CompilationError where show = unCompilationError

check :: Exp -> Maybe CompilationError
check exp = (CompilationError . show) <$> checkClosed exp

compile :: Exp -> IO (Either CompilationError Code)
compile exp = return $
  case checkClosed exp of
    Just err -> Left $ CompilationError $ show err
    Nothing -> Right $ flatten exp

execute :: Code -> (Value,Instrumentation)
execute code = (evaluate code, ())

type Instrumentation = ()
