
module Pipeline3 (CompilationError,Code,Value,Instrumentation,check,compile,execute,Opt(..)) where

import Control.Exception(try)
import qualified System.Console.ANSI as AN

-- Pipeline: Ast -> Anf -> ClosureConverted

import Builtin (CommandLineArgs)
import Rep_Ast (Exp)
import Eval_ClosureConverted (Value,Instrumentation)
import CheckClosed_Ast (checkClosed)
import RuntimeCallingConventions (RT)
import Trans_Normalize (normalize)
import Trans_Ast2Anf (flatten)
import Trans_Anf2CC (convert)
import qualified Eval_ClosureConverted as Eval (execute)
import qualified Rep_ClosureConverted as Clo

data Opt = NoOpt | NbE

data CompilationError = CompilationError { unCompilationError :: String }
instance Show CompilationError where show = unCompilationError

check :: Exp -> Maybe CompilationError
check exp = (CompilationError . show) <$> checkClosed exp

data Code = Code { cc :: Clo.Code, rt :: RT }

-- quick hackto allow switch NBE on/off -- TODO: tidy this up
-- TODO: compile should redo the check
compile :: RT -> Opt -> Exp -> IO (Either CompilationError Code)
compile rt opt exp = do
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
      let cc = convert rt anf
      put $ col AN.Magenta (show cc)
      return $ Right $ Code {cc,rt}
  where
    put :: String -> IO ()
    put = if optPut then putStr else \_ -> return ()

    optPut = False  -- TODO: select via opt

execute :: CommandLineArgs -> Code -> IO (Either String (Value, Instrumentation))
execute cla Code{cc,rt} = do
  either (Left . show) Right <$> (try @IOError $ Eval.execute rt cla cc)

col :: AN.Color -> String -> String
col c s =
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid c] <> s <>
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid AN.White]
