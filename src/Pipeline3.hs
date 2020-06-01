
module Pipeline3 (CompilationError,Code,Value,Instrumentation,check,compile,execute,quietCompile) where

import qualified System.Console.ANSI as AN

-- Pipeline: Ast -> Anf -> ClosureConverted

import Rep_Ast (Exp)
import Rep_ClosureConverted (Code)
import Eval_ClosureConverted (Value,Instrumentation)
import CheckClosed_Ast (checkClosed)
import Trans_Normalize (normalize)
import Trans_Ast2Anf (flatten)
import Trans_Anf2CC (convert)
import Trans_CC2Linear (linearize)
import Eval_ClosureConverted (execute)
import qualified Rep_Linear as Lin

data CompilationError = CompilationError { unCompilationError :: String }
instance Show CompilationError where show = unCompilationError

check :: Exp -> Maybe CompilationError
check exp = (CompilationError . show) <$> checkClosed exp

-- quick hackto allow switch NBE on/off -- TODO: tidy this up
-- TODO: compile should redo the check
compile :: Bool -> Exp -> IO (Either CompilationError Code)
compile nbe exp = do
  case checkClosed exp of
    Just err -> return $ Left $ CompilationError $ show err
    Nothing -> do
      putStr $ col AN.Yellow (show exp)
      exp' <-
        if not nbe then pure exp else do
          let exp' = normalize exp
          putStr $ col AN.Green (show exp')
          pure exp'
      let anf = flatten exp'
      putStr $ col AN.Blue (show anf)
      let cc = convert anf
      putStr $ col AN.Magenta (show cc)
      let lin = linearize cc
      putStr $ col AN.White (show lin)
      return $ Right cc


col :: AN.Color -> String -> String
col c s =
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid c] <> s <>
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid AN.White]


quietCompile :: Exp -> Lin.Code
quietCompile exp = do
  let exp' = normalize exp
  let anf = flatten exp'
  let cc = convert anf
  let lin = linearize cc
  lin
