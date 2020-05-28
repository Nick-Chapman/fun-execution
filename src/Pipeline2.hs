
module Pipeline2(Env,Def(..),Exp,CompilationError,Code,Value,Instrumentation,parse,compile,execute,env0) where

import qualified Data.Map.Strict as Map

-- pipeline: Ast->Anf

import CheckClosed_Ast (checkInEnv)
import Eval_Anf (Value,Env)
import Parse (parse)
import Rep_Anf (Code)
import Rep_Ast (Def(..),Exp)
import Trans_Ast2Anf (flatten)
import qualified Eval_Anf as Anf
import qualified Rep_Ast as Ast(env0)


data CompilationError = CompilationError { unCompilationError :: String }
instance Show CompilationError where show = unCompilationError

compile :: Exp -> Either CompilationError Code
compile exp =
  case checkInEnv [] exp of
    Just err -> Left $ CompilationError $ show err
    Nothing -> Right $ flatten exp

execute :: Code -> (Value,Instrumentation)
execute code = (Anf.evaluate Map.empty code, ())

env0 :: Env
env0 = Map.map eval Ast.env0 where
  eval = fst . execute . getRight . compile

getRight :: Show e => Either e a -> a
getRight = either (error . show) id

type Instrumentation = ()
