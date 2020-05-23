
-- Link together the stages of the compilation pipeline
module Pipeline(Env,Def(..),Exp,Code,Value,parse,compile,execute,env0) where

import Eval_Basic(RuntimeError,evaluate,env0)
import Parse(parse)
import Rep_Basic(Value,Env)
import Rep_Lang (Exp,Def(..))
import Trans_Lang2Basic(translate)
import qualified Rep_Basic as Basic(Exp)

data Code = Code Basic.Exp
instance Show Code where show (Code e) = show e

type CompilationError = () -- not yet possible, as simplify Lang->Basic is total

compile :: Exp -> Either CompilationError Code
compile = Right . Code . translate

execute :: Env -> Code -> Either RuntimeError Value
execute env (Code e) = evaluate env e
