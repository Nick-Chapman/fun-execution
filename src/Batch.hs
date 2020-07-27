
module Batch(main) where

import Parse (parse)
import Pipeline (check,quietCompile,Opt(..))
import Rep1_Ast (Def(..),wrapDef)
import System.Environment (getArgs)
import qualified Data.List as List
import qualified Predefined (defs)
import qualified Rep4_Lin as Lin
import qualified System.Console.ANSI as AN

main :: IO ()
main = do
  getArgs >>= \case
    -- TODO: better argument parsing!
    ["-view","-nn",infile,outfile] -> batch True NoOpt infile outfile
    ["-view",infile,outfile] -> batch True NbE infile outfile
    ["-nn",infile,outfile] -> batch False NoOpt infile outfile
    [infile,outfile] -> batch False NbE infile outfile
    xs -> error $ "Batch.main: unexpected args: " ++ show xs

batch :: Bool -> Opt -> FilePath -> FilePath -> IO ()
batch view opt i o = do
  s <- readFile i
  lin <- compileProgLines view opt $ lines s
  writeFile o (show lin)

compileProgLines :: Bool -> Opt -> [String] ->  IO Lin.Code
compileProgLines view opt xs = loop Predefined.defs xs
  where
    loop defs = \case
      line:rest -> do
        defs <- compileLine defs line
        loop defs rest
      [] -> do
        case defs of
          [] -> error "found no defs"
          Def name exp : _ -> do
            putStrLn $ "compiling definition: " ++ show name
            let expWithContext = List.foldl (flip wrapDef) exp defs
            quietCompile view opt expWithContext

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
