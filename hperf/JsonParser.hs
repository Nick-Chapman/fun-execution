
module JsonParser (pipeline) where

import Prelude hiding (null,length)

import Control.Monad(ap,liftM)
import Data.List (length)


-- Pipeline: generate JSON; convert to string; parse; eval
pipeline :: Int -> Int
pipeline n = do
  let ast0 = nfibJ n -- generate
  let res0 = eval ast0
  let str = show ast0 -- convert to string
  let len = length str
  let ast1 = parse str
  let res1 = eval ast1
  if (res1 /= res0) then error "res1 <> res0" else
    len

-- Construct an arithmetic expression for an nfib calculation in JSON
nfibJ :: Int -> Json
nfibJ 0 = lit 1
nfibJ 1 = lit 1
nfibJ n = add (lit 1) (add (nfibJ (n-1)) (nfibJ (n-2)))


{-
-- Constructors for JSON rep of simple arithmetic expressions
lit :: Int -> Json
lit n = Jobject [("lit", Jnumber n),("extra",Jstring"info")]

add :: Json -> Json -> Json
add x y = Jobject [("op", Jarray [x,y]), ("isAdd",Jtrue)]

-- Evaluate JSON represented arithmetic expressions
eval :: Json -> Int
eval = \case
  Jobject [("lit", Jnumber n),("extra",Jstring _)] -> n
  Jobject [("op", Jarray [x,y]), ("isAdd",Jtrue)] -> eval x + eval y
  Jobject [("op", Jarray [x,y]), ("isAdd",Jfalse)] -> eval x - eval y
  x -> error $ "eval unexpected ast: " ++ show x
-}



-- Match the .fun version...

-- Constructors for JSON rep of simple arithmetic expressions

lit :: Int -> Json
lit n = Jobject [("lit", Jnumber n)]

add :: Json -> Json -> Json
add x y = Jobject [("add", Jarray [x,y])]


-- Evaluate JSON represented arithmetic expressions
eval :: Json -> Int
eval = \case
  Jobject [("lit", Jnumber n)] -> n
  Jobject [("add", Jarray [x,y])] -> eval x + eval y
  x -> error $ "eval unexpected ast: " ++ show x


-- AST for JSON
type Member = (String,Json)
data Json
  = Jobject [Member]
  | Jarray [Json]
  | Jstring String
  | Jnumber Int
  | Jtrue
  | Jfalse
  | Jnull


-- Convert JSON to a string: Collect all the text chunks and implode them at the end
instance Show Json where
  show json = concat (reverse (chunks [] json))

chunks :: [String] -> Json -> [String]
chunks acc = \case
  Jobject [] -> "{}" : acc
  Jobject (x:xs) ->
    " }" : foldl (\acc x -> chunksMember (", " : acc) x) (chunksMember ("{ " : acc) x) xs
  Jarray [] -> "[]" : acc
  Jarray (x:xs) ->
    " ]" : foldl (\acc x -> chunks (", " : acc) x) (chunks ("[ " : acc) x) xs
  Jstring s -> show s : acc
  Jnumber n -> show n : acc
  Jtrue -> "true" : acc
  Jfalse -> "false" : acc
  Jnull -> "null" : acc

  where
    chunksMember :: [String] -> Member -> [String]
    chunksMember acc (k,v) = chunks (": " : (show k : acc)) v


-- Parse a string into JSON
parse :: String -> Json
parse = parseWith json


-- JSON gramar, following: https://www.json.org/json-en.html

json :: Par Json
json = do ws; fixP (\element -> recursiveStructure element)

recursiveStructure :: Par Json -> Par Json
recursiveStructure element0 = element where

  value,object,array,element :: Par Json
  elements :: Par [Json]
  members :: Par [Member]
  member :: Par Member

  element = do
    v <- value
    ws; return v

  value = alts [object,array,string,number,true,false,null]

  object = do
    char '{'
    ws; ms <- alts [return [], members]
    char '}'
    return $ Jobject ms

  members = commaSeparated member

  member = do
    s <- str
    ws; char ':'
    ws; v <- element0
    return (s,v)

  array = do
    char '[';
    ws; es <- alts [return [], elements]
    char ']';
    return $ Jarray es

  elements = commaSeparated element0


string,number,true,false,null :: Par Json
integer,positive :: Par Int
str,characters :: Par String
character,digit,onenine :: Par Char

true = do keyword "true"; return Jtrue
false = do keyword "false"; return Jfalse
null = do keyword "null"; return  Jnull

string = Jstring <$> str

str = do
  char '"'
  cs <- characters
  char '"'
  return cs

characters = many character

character = satisfy $ \c ->
  c >= ' ' && c `notElem` ['"','\\'] -- escaping not supported

number = Jnumber <$> integer -- fraction/exponent not supported

integer = alts
  [ positive
  , do char '-'; negate <$> positive
  ]

positive = read <$> alts
  [ do char '0'; return "0"
  , do d <- onenine; ds <- many digit; return (d : ds)
  ]

digit = satisfy $ \c -> c >= '0' && c <= '9'
onenine = satisfy $ \c -> c >= '1' && c <= '9'


commaSeparated :: Par a -> Par [a]
commaSeparated thing = do
  x <- thing
  xs <- alts [return [], do char ','; ws; commaSeparated thing]
  return (x:xs)

ws :: Par ()
ws = fixP $ \ws -> alts
  [ return ()
  , do char ' '; ws
  , do char '\n'; ws
  , do char '\t'; ws
  ]

keyword :: String -> Par ()
keyword = mapM_ char

char :: Char -> Par ()
char x = do _ <- satisfy (== x); return ()

alts :: [Par a] -> Par a
alts = foldl altP failP

many :: Par a -> Par [a]
many thing = alts [return [], do x <- thing; xs <- many thing; return (x:xs)]


instance Functor Par where fmap = liftM
instance Applicative Par where pure = retP; (<*>) = ap
instance Monad Par where (>>=) = bindP


-- "Parser combinators need four values to report errors", Andrew Partridge & David Wright

newtype Par a = Par (forall b. [Char] -> K4 a b -> Res b)

runPar :: Par a -> [Char] -> K4 a b -> Res b
runPar (Par f) = f

type Res a = Either [Char] (a,[Char])

data K4 a b = K4 -- Four continuations:
  { eps :: a -> Res b            -- success; *no* input consumed
  , succ :: [Char] -> a -> Res b -- success; input consumed
  , fail :: () -> Res b          -- failure; *no* input consumed
  , err :: [Char] -> Res b       -- failure; input consumed (so an error!)
  }


parseWith :: Par a -> String -> a
parseWith (Par p) chars = finalize (p chars kFinal) where

  finalize :: Res a -> a
  finalize = \case
    Left remains -> error $ "failed to parse at position: " ++ show (len - length remains)
    Right (a,[]) -> a
    Right (_,remains) -> error $ "unparsed input remains at: " ++ show (len - length remains)

  len = length chars

  kFinal = K4 { eps = \a -> Right (a,chars)
              , succ = \chars a -> Right (a,chars)
              , fail = \() -> Left chars
              , err = \chars -> Left chars
              }

fixP :: (Par a -> Par a) -> Par a
fixP f = Par (\chars k -> runPar (f (fixP f)) chars k)

retP :: a -> Par a
retP x = Par (\_chars K4{eps} -> eps x)

failP :: Par a
failP = Par (\_chars K4{fail} -> fail ())

satisfy :: (Char -> Bool) -> Par Char
satisfy pred = Par (
  \chars K4{succ,fail} -> do
    case chars of
      [] -> fail ()
      c:chars -> if pred c then succ chars c else fail ())

altP :: Par a -> Par a -> Par a
altP (Par p1) (Par p2) = Par (
  \chars k@K4{eps,succ,err} ->
    p1 chars K4{ eps = \a1 ->
                     p2 chars K4{ eps = \_ -> eps a1 -- left biased
                                  , succ
                                  , fail = \() -> eps a1
                                  , err
                                  }
                 , succ
                 , fail = \() -> p2 chars k
                 , err
                 })

bindP :: Par a -> (a -> Par b) -> Par b
bindP (Par p1) f2 = Par (
  \chars k@K4{succ,fail,err} ->
    p1 chars K4{ eps = \a -> runPar (f2 a) chars k
                 , succ = \chars a ->
                            runPar (f2 a) chars K4{ eps = \a -> succ chars a -- consume
                                                    , succ
                                                    , fail = \() -> err chars -- fail->err
                                                    , err
                                                    }
                 , fail
                 , err
                 })
