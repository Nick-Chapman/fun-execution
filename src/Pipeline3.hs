
module Pipeline3 (CompilationError,Code,Value,Instrumentation,check,compile,execute) where

import qualified System.Console.ANSI as AN

-- Pipeline: Ast -> Anf -> ClosureConverted

import Rep_Ast (Exp)
import Rep_ClosureConverted (Code)
import Eval_ClosureConverted (Value,Instrumentation)
import CheckClosed_Ast (checkClosed)
import Trans_Normalize (normalize)
import Trans_Ast2Anf (flatten)
import Trans_Anf2CC (convert)
import Eval_ClosureConverted (execute)

data CompilationError = CompilationError { unCompilationError :: String }
instance Show CompilationError where show = unCompilationError

check :: Exp -> Maybe CompilationError
check exp = (CompilationError . show) <$> checkClosed exp

compile :: Exp -> IO (Either CompilationError Code)
compile exp = do
  case checkClosed exp of
    Just err -> return $ Left $ CompilationError $ show err
    Nothing -> do
      putStr $ col AN.Yellow (show exp)
      let exp' = normalize exp
      putStr $ col AN.Green (show exp')
      let anf = flatten exp'
      putStr $ col AN.Blue (show anf)
      let code = convert anf
      putStr $ col AN.Magenta (show code)
      return $ Right code


col :: AN.Color -> String -> String
col c s =
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid c] <> s <>
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid AN.White]
