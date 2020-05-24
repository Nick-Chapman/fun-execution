
-- Link together the stages of the compilation pipeline
module Pipeline(Env,Def(..),Exp,Code,Value,parse,compile,execute,env0) where

import Eval_Ast (RuntimeError,evaluate,env0)
import Parse (parse)
import Rep_Ast as Ast(Value,Env,Exp,Def(..))

data Code = Code Exp
instance Show Code where show (Code e) = show e

type CompilationError = () -- not yet possible, as simplify Lang->Basic is total

compile :: Exp -> Either CompilationError Code
compile = Right . Code

execute :: Env -> Code -> Either RuntimeError Value
execute env (Code exp) = evaluate env exp
