
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

-- unit pipeline: Ast is Code; compilation is closeOverEnvironment
{-
import Close_Ast (UnboundError,closeOverEnvironment)
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
-}

-- pipeline: Ast->Anf

import Parse (parse)
import Rep_Ast (Def(..),Exp)
import Rep_Anf (Code,Value,Env)
import Trans_Ast2Anf (flatten,flattenV)
import Eval_Anf (RuntimeError,eval)
import qualified Rep_Ast as Ast(env0)

env0 :: Env
env0 = Map.map flattenV Ast.env0

data CompilationError = CE deriving Show -- none yet

compile :: Env -> Exp -> Either CompilationError Code
compile env exp = Right $ flatten env exp

execute :: Code -> Either RuntimeError Value
execute = eval
