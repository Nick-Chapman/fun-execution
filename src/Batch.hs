
module Batch(main) where

import System.Environment (getArgs)
import qualified Data.List as List
import qualified System.Console.ANSI as AN

import Parse (parse)
import Pipeline3 (check,quietCompile)
import Rep_Ast (Def(..),wrapDef)
import qualified Predefined (defs)
import qualified Rep_Linear as Lin

main :: IO ()
main = do
  base <- getArgs >>= \case
    [x] -> return x
    xs -> error $ "unexpected args: " ++ show xs
  let infile = "fun/"++base++".fun"
  let outfile = "gen/"++base++".c"
  batch infile outfile

batch :: FilePath -> FilePath -> IO ()
batch i o = do
  s <- readFile i
  lin <- compileProgLines $ lines s
  writeFile o (show lin)

compileProgLines :: [String] ->  IO Lin.Code
compileProgLines xs = loop Predefined.defs xs
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
            let lin = quietCompile expWithContext
            return lin

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
