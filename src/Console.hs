
module Console(main) where

import Control.Monad.Trans.Class (lift)
import qualified Data.List as List
import System.Environment (getArgs)
import qualified Data.Map.Strict as Map
import qualified System.Console.ANSI as AN
import qualified System.Console.Haskeline as HL
import qualified System.Console.Haskeline.History as HL

import Pipeline (parse,Def(..),Exp,Value,Instrumentation,compile,execute)

import Rep_Ast (wrapDef)
import qualified Rep_Ast as Ast

main :: IO ()
main = do
  args <- getArgs
  let conf = parseArgs args
  HL.runInputT haskelineSettings $ start conf

data Conf = Conf
  { funFile :: FilePath
  , verbose :: Bool -- what should this control?
  }

defaultConf :: Conf
defaultConf = Conf
  { funFile = ".history"
  , verbose = False
  }

parseArgs :: [String] -> Conf
parseArgs args = loop args defaultConf
  where
    loop args conf = case args of
      [] -> conf
      "-v":rest -> loop rest $ conf { verbose = True }
      funFile:rest -> loop rest $ conf { funFile }

start :: Conf -> HL.InputT IO ()
start conf = do
  history <- lift $ readHistory conf
  HL.putHistory history
  defs <- lift $ replay conf defs0 (HL.historyLines history)
  repl conf 1 defs

defs0 :: [Def]
defs0 = [ Def x rhs | (x,rhs) <- Map.toList Ast.env0 ]

-- keep history in opposite order from HL standard (newest at end of file)

haskelineSettings :: HL.Settings IO
haskelineSettings = HL.defaultSettings {HL.autoAddHistory = False}

revHistory :: HL.History -> HL.History
revHistory = foldl (flip HL.addHistory) HL.emptyHistory . HL.historyLines

writeHistory :: Conf -> HL.History -> IO ()
writeHistory Conf{funFile} = HL.writeHistory funFile . revHistory

readHistory :: Conf -> IO HL.History
readHistory Conf{funFile} = fmap revHistory $ HL.readHistory funFile

-- replay .history lines
replay :: Conf -> [Def] -> [String] -> IO [Def]
replay conf defs = \case
  [] -> return defs
  line:earlier -> do
    defs1 <- replay conf defs earlier
    pep conf putStrLn line defs1 >>= \case
      Nothing -> return defs1
      Just defs2 -> return defs2

-- read-eval-print-loop
repl :: Conf -> Int -> [Def] -> HL.InputT IO ()
repl conf n defs = do
  HL.getInputLine (col AN.Green $ show n <> "> ") >>= \case
    Nothing -> return ()
    Just line -> do
      HL.modifyHistory (HL.addHistory line)
      HL.getHistory >>= lift . writeHistory conf
      let noput _ = return ()
      lift (pep conf noput line defs) >>= \case
        Nothing -> repl conf n defs
        Just defs' -> repl conf (n + 1) defs'

-- parse-eval-print
pep :: Conf -> (String -> IO ()) -> String -> [Def] -> IO (Maybe [Def])
pep Conf{} put line defs = do
  case parse line of

    Left err -> do
      putStrLn $ col AN.Red $ "parse error: " <> show err <> " : " <> line
      return Nothing

    Right Nothing -> do
      return Nothing

    Right (Just (Left (def@(Def name exp)))) -> do
      put line
      eval defs exp >>= \case
        Nothing -> return Nothing
        Just (value,instrumentation) -> do
          putStrLn $ col AN.Cyan (show name <> " = " <> show value)
          putStrLn $ col AN.Green (show instrumentation)
          return $ Just (def : defs)

    Right (Just (Right exp)) -> do
      put line
      eval defs exp >>= \case
        Nothing -> return Nothing
        Just (value,instrumentation) -> do
          putStrLn $ col AN.Cyan (show value)
          putStrLn $ col AN.Green (show instrumentation)
          return Nothing

eval :: [Def] -> Exp -> IO (Maybe (Value,Instrumentation))
eval defs exp = do
  putStr $ col AN.Magenta (show exp)
  let expWithContext = List.foldl (flip wrapDef) exp defs
  case compile expWithContext of
    Left err -> do
      putStrLn $ col AN.Red (show err)
      return Nothing
    Right code -> do
      putStr $ col AN.Blue (show code)
      return $ Just $ execute code

col :: AN.Color -> String -> String
col c s =
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid c] <> s <>
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid AN.White]
