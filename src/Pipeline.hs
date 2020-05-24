
-- Link together the stages of the compilation pipeline
module Pipeline(Env,Def(..),Exp,Code,Value,parse,compile,execute,env0) where

import Close_Ast
import Eval_Ast (RuntimeError)
import Parse (parse)
import Rep_Ast as Ast (Value,Env,Exp,Def(..),env0)
import qualified Eval_Ast as Ast

data Code = Code Exp
instance Show Code where show (Code e) = show e

type CompilationError = UnboundError

compile :: Env -> Exp -> Either CompilationError Code
compile env exp = either Left (Right . Code) $ closeOverEnvironment exp env

execute :: Code -> Either RuntimeError Value
execute (Code exp) = Ast.execute exp
