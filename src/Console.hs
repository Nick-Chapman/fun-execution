
module Console(main) where

import Control.Monad.Trans.Class (lift)
import System.Environment (getArgs)
import qualified Data.Map.Strict as Map
import qualified System.Console.ANSI as AN
import qualified System.Console.Haskeline as HL
import qualified System.Console.Haskeline.History as HL

import Pipeline(Env,Def(..),Exp,Value,Instrumentation,parse,compile,execute,env0)

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
  env <- lift $ replay conf env0 (HL.historyLines history)
  repl conf 1 env

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
replay :: Conf -> Env -> [String] -> IO Env
replay conf env = \case
  [] -> return env
  line:earlier -> do
    env1 <- replay conf env earlier
    pep conf putStrLn line env1 >>= \case
      Nothing -> return env1
      Just env2 -> return env2

-- read-eval-print-loop
repl :: Conf -> Int -> Env -> HL.InputT IO ()
repl conf n env = do
  HL.getInputLine (col AN.Green $ show n <> "> ") >>= \case
    Nothing -> return ()
    Just line -> do
      HL.modifyHistory (HL.addHistory line)
      HL.getHistory >>= lift . writeHistory conf
      let noput _ = return ()
      lift (pep conf noput line env) >>= \case
        Nothing -> repl conf n env
        Just env' -> repl conf (n + 1) env'


-- parse-eval-print
pep :: Conf -> (String -> IO ()) -> String -> Env -> IO (Maybe Env)
pep Conf{} put line env = do
  case parse line of

    Left err -> do
      putStrLn $ col AN.Red $ "parse error: " <> show err <> " : " <> line
      return Nothing

    Right Nothing -> do
      return Nothing

    Right (Just (Left (Def name exp))) -> do
      put line
      eval env exp >>= \case
        Nothing -> return Nothing
        Just (value,instrumentation) -> do
          putStrLn $ col AN.Cyan (show name <> " = " <> show value)
          putStrLn $ col AN.Green (show instrumentation)
          return $ Just $ Map.insert name value env

    Right (Just (Right exp)) -> do
      put line
      eval env exp >>= \case
        Nothing -> return Nothing
        Just (value,instrumentation) -> do
          putStrLn $ col AN.Cyan (show value)
          putStrLn $ col AN.Green (show instrumentation)
          return Nothing

eval :: Env -> Exp -> IO (Maybe (Value,Instrumentation))
eval env exp = do
  putStrLn $ col AN.Magenta ("EXP:" <> show exp)
  case compile env exp of
    Left err -> do
      putStrLn $ col AN.Red (show err)
      return Nothing
    Right code -> do
      putStrLn $ col AN.Blue (show code)
      case execute env code of
        Left err -> do
          putStrLn $ col AN.Red (show err)
          return Nothing
        Right vi -> do
          return $ Just vi


col :: AN.Color -> String -> String
col c s =
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid c] <> s <>
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid AN.White]
