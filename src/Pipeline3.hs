
module Pipeline3 (CompilationError,Code,Value,Instrumentation,check,compile,execute,quietCompile,Opt(..)) where

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

data Opt = NoOpt | NbE

data CompilationError = CompilationError { unCompilationError :: String }
instance Show CompilationError where show = unCompilationError

check :: Exp -> Maybe CompilationError
check exp = (CompilationError . show) <$> checkClosed exp

-- quick hackto allow switch NBE on/off -- TODO: tidy this up
-- TODO: compile should redo the check
compile :: Opt -> Exp -> IO (Either CompilationError Code)
compile opt exp = do
  case checkClosed exp of
    Just err -> return $ Left $ CompilationError $ show err
    Nothing -> do
      put $ col AN.Yellow (show exp)
      exp' <-
        case opt of
          NoOpt -> pure exp
          NbE -> do
            let exp' = normalize exp
            put $ col AN.Green (show exp')
            pure exp'
      let anf = flatten exp'
      put $ col AN.Blue (show anf)
      let cc = convert anf
      put $ col AN.Magenta (show cc)
      let lin = linearize cc
      put $ col AN.White (show lin)
      return $ Right cc
  where
    put :: String -> IO ()
    put = if optPut then putStr else \_ -> return ()

    optPut = False  -- TODO: select via opt

col :: AN.Color -> String -> String
col c s =
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid c] <> s <>
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid AN.White]


quietCompile :: Opt -> Exp -> Lin.Code
quietCompile opt exp = do
  let exp' = case opt of NoOpt -> exp; NbE -> normalize exp
  let anf = flatten exp'
  let cc = convert anf
  let lin = linearize cc
  lin
