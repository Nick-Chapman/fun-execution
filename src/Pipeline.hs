
-- Link together the stages of the compilation pipeline
module Pipeline(Env,Def(..),Exp,Code,Value,parse,compile,execute,env0) where

import qualified Data.Map.Strict as Map

-- dummy pipeline from Ast
{-
import Parse (parse)
import Rep_Ast (Value,Env,Exp,Def(..),env0)
data CompilationError = CE deriving Show
data Code = Code deriving Show
compile :: Env -> Exp -> Either CompilationError Code
compile = undefined
data RuntimeError = RE deriving Show
execute :: Code -> Either RuntimeError Value
execute = undefined
-}

-- unit pipeline: Ast is Code; compilation is just to check for unbound identifiers
{-
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
compile env exp = maybe (Right $ Code exp) (Left . CompilationError . show) $ checkInEnv (Map.keys env) exp
execute :: Env -> Code -> Either RuntimeError Value
execute env (Code exp) = Ast.evaluate env exp
env0 :: Eval_Ast.Env
env0 = Map.map eval Ast.env0 where eval = either (error . show) id . Ast.evaluate Map.empty
-}


-- pipeline: Ast->Anf

import CheckClosed_Ast (checkInEnv)
import Parse (parse)
import Rep_Ast (Def(..),Exp)
import Rep_Anf (Code,Value,Env)
import Trans_Ast2Anf (flatten)
import Eval_Anf(RuntimeError)
import qualified Eval_Anf as Anf
import qualified Rep_Ast as Ast(env0)

env0 :: Env
env0 = Map.map eval Ast.env0
  where eval = getRight . execute Map.empty . getRight . compile Map.empty

getRight :: Show e => Either e a -> a
getRight = either (error . show) id

data CompilationError = CompilationError { unCompilationError :: String }
instance Show CompilationError where show = unCompilationError

compile :: Env -> Exp -> Either CompilationError Code
compile env exp =
  case checkInEnv (Map.keys env) exp of
    Just err -> Left $ CompilationError $ show err
    Nothing -> Right $ flatten env exp

execute :: Env -> Code -> Either RuntimeError Value
execute = Anf.evaluate
