
module Pipeline1(Env,Def(..),Exp,Code,Value,Instrumentation,parse,compile,execute,env0) where

import qualified Data.Map.Strict as Map

-- trivial pipeline: Ast is Code; compilation is just to check for unbound identifiers

import CheckClosed_Ast (checkInEnv)
import Eval_Ast (RuntimeError,Value,Env)
import Parse (parse)
import Rep_Ast (Exp,Def(..))
import qualified Eval_Ast as Ast (evaluate)
import qualified Rep_Ast as Ast (env0)

data Code = Code Exp
instance Show Code where show (Code e) = "CODE:" <> show e

data CompilationError = CompilationError { unCompilationError :: String }
instance Show CompilationError where show = unCompilationError

compile :: Env -> Exp -> Either CompilationError Code
compile env exp =
  case checkInEnv (Map.keys env) exp of
    Just err -> Left $ CompilationError $ show err
    Nothing -> Right (Code exp)

execute :: Env -> Code -> Either RuntimeError Value
execute env (Code exp) = Ast.evaluate env exp

env0 :: Env
env0 = Map.map eval Ast.env0 where
  eval = getRight . Ast.evaluate Map.empty

getRight :: Show e => Either e a -> a
getRight = either (error . show) id

type Instrumentation = ()
