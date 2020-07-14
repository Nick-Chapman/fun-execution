
module Batch(main) where

import System.Environment (getArgs)
import qualified Data.List as List
import qualified System.Console.ANSI as AN

import Parse (parse)
import Pipeline (check,quietCompile,Opt(..))
import Rep1_Ast (Def(..),wrapDef)
import RuntimeCallingConventions (RT(..),ContFreeVars(..))
import qualified Predefined (defs)
import qualified Rep4_Lin as Lin

main :: IO ()
main = do
  let rtFos = RT { contFreeVars = FOS }
  let rtFif = RT { contFreeVars = FIF }
  getArgs >>= \case
    -- TODO: better argument parsing!
    ["-nn","-fos",infile,outfile] -> batch rtFos NoOpt infile outfile
    ["-fos",infile,outfile] -> batch rtFos NbE infile outfile
    ["-nn",infile,outfile] -> batch rtFif NoOpt infile outfile
    [infile,outfile] -> batch rtFif NbE infile outfile
    xs -> error $ "Batch.main: unexpected args: " ++ show xs

batch :: RT -> Opt -> FilePath -> FilePath -> IO ()
batch rt opt i o = do
  s <- readFile i
  lin <- compileProgLines rt opt $ lines s
  writeFile o (show lin)

compileProgLines :: RT -> Opt -> [String] ->  IO Lin.Code
compileProgLines rt opt xs = loop Predefined.defs xs
  where
    loop defs = \case
      line:rest -> do
        defs <- compileLine defs line
        loop defs rest
      [] -> do
        case defs of
          [] -> error "found no defs"
          Def name exp : _ -> do
            putStrLn $ "compiling (" ++ show (contFreeVars rt) ++ ") definition: " ++ show name
            let expWithContext = List.foldl (flip wrapDef) exp defs
            quietCompile rt opt expWithContext

compileLine :: [Def] -> String -> IO [Def]
compileLine defs line = do
  case parse line of
    Left err -> do
      putStrLn $ col AN.Red $ "parse error: " <> show err <> " : " <> line
      return defs
    Right Nothing -> do
      return defs
    Right (Just (Left (def@(Def _ exp)))) -> do
      --putStrLn line
      let expWithContext = List.foldl (flip wrapDef) exp defs
      case check expWithContext of
        Just err -> do
          putStrLn $ col AN.Red (show err)
          return defs
        Nothing ->
          return (def : defs)
    Right (Just (Right _)) -> do
      --putStrLn line
      --putStrLn $ col AN.Red $ "ignoring top level expression"
      return defs

col :: AN.Color -> String -> String
col c s =
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid c] <> s <>
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid AN.White]
