
module Pipeline1(Env,Def(..),Exp,CompilationError,Code,Value,Instrumentation,parse,compile,execute,env0) where

import qualified Data.Map.Strict as Map

-- trivial pipeline: Ast is Code; compilation is just to check for unbound identifiers

import CheckClosed_Ast (checkInEnv)
import Eval_Ast (Value,Env)
import Parse (parse)
import Rep_Ast (Exp,Def(..),indented,pretty)
import qualified Eval_Ast as Ast (evaluate)
import qualified Rep_Ast as Ast (env0)

data Code = Code Exp
instance Show Code where show (Code e) = unlines $ indented "CODE:" (pretty e)

data CompilationError = CompilationError { unCompilationError :: String }
instance Show CompilationError where show = unCompilationError

compile :: Exp -> Either CompilationError Code
compile exp =
  case checkInEnv [] exp of
    Just err -> Left $ CompilationError $ show err
    Nothing -> Right (Code exp)

execute :: Code -> (Value,Instrumentation)
execute (Code exp) = (Ast.evaluate Map.empty exp, ())

env0 :: Env
env0 = Map.map eval Ast.env0 where
  eval = fst . execute . Code

type Instrumentation = ()
