
module Pipeline3(Env,Def(..),Exp,Code,Value,parse,compile,execute,env0) where

import qualified Data.Map.Strict as Map

-- Pipeline: Ast -> Anf -> ClosureConverted

import CheckClosed_Ast (checkInEnv)
import Eval_Anf (RuntimeError)
import Eval_ClosureConverted (Value)
import Parse (parse)
import Rep_Ast (Def(..),Exp)
import Rep_ClosureConverted (Code)
import Trans_Anf2CC (Env,convert)
import Trans_Ast2Anf (flatten)
import qualified Eval_ClosureConverted as CC
import qualified Rep_Ast as Ast (env0)

data CompilationError = CompilationError { unCompilationError :: String }
instance Show CompilationError where show = unCompilationError

compile :: Env -> Exp -> Either CompilationError Code
compile env exp =
  case checkInEnv (Map.keys env) exp of
    Just err -> Left $ CompilationError $ show err
    Nothing -> Right $ convert env $ flatten exp

execute :: Env -> Code -> Either RuntimeError Value
execute _env code = Right v where CC.Result v _ = CC.execute code

env0 :: Env
env0 = Map.map eval Ast.env0 where
  eval = getRight . execute Map.empty . getRight . compile Map.empty

getRight :: Show e => Either e a -> a
getRight = either (error . show) id
