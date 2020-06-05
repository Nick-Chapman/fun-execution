
module JsonParserGADT (pipeline) where

import Prelude hiding (fail,null)

import Control.Monad(ap,liftM)


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


-- Construct a jsonified arithmetic expressions for an nfib calculation
nfibJ :: Int -> Json
nfibJ 0 = lit 1
nfibJ 1 = lit 1
nfibJ n = add (lit 1) (add (nfibJ (n-1)) (nfibJ (n-2)))


-- Json rep for simple arithmetic expressions: construct/eval
lit :: Int -> Json
lit n = Jobject [("lit", Jnumber n),("extra",Jstring"info")]

add :: Json -> Json -> Json
add x y = Jobject [("op", Jarray [x,y]), ("isAdd",Jtrue)]

eval :: Json -> Int
eval = \case
  Jobject [("lit", Jnumber n),("extra",Jstring _)] -> n
  Jobject [("op", Jarray [x,y]), ("isAdd",Jtrue)] -> eval x + eval y
  Jobject [("op", Jarray [x,y]), ("isAdd",Jfalse)] -> eval x - eval y
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

-- JSON gramar, broadly following: https://www.json.org/json-en.html

parse :: String -> Json
parse = parseWith json


json :: Par Json
json = (do ws; element) where

  value = alts [object,array,string,number,true,false,null]

  true = do key "true"; return Jtrue
  false = do key "false"; return Jfalse
  null = do key "null"; return  Jnull

  object = do
    char '{';
    ws; ms <- alts [return [], members]
    char '}';
    return $ Jobject ms

  members = do
    x <- member
    xs <- alts [return [], do char ','; ws; members]
    return (x:xs)

  member = do
    s <- str
    ws; char ':'
    ws; v <- element
    return (s,v)

  array = do
    char '[';
    ws; es <- alts [return [], elements]
    char ']';
    return $ Jarray es

  elements = do
    x <- element
    xs <- alts [return [], do char ','; ws; elements]
    return (x:xs)

  element = do
    v <- value
    ws; return v

  string = Jstring <$> str

  str = do
    char '"'
    cs <- characters
    char '"'
    return cs

  characters = many character

  character = Satisfy $ \c ->
    c >= ' ' && c `notElem` ['"','\\'] -- escaping not supported

  number = Jnumber <$> integer -- fraction/exponent not supported

  integer = alts
    [ positive
    , do char '-'; negate <$> positive
    ]

  positive = read <$> alts
    [ do char '0'; return ['0']
    , do d <- onenine; ds <- many digit; return (d:ds)
    ]

  digit = Satisfy $ \c -> c >= '0' && c <= '9'
  onenine = Satisfy $ \c -> c >= '1' && c <= '9'

  ws :: Par ()
  ws = alts
    [ return ()
    , do char ' '; ws
    , do char '\n'; ws
    , do char '\t'; ws
    ]

key :: String -> Par ()
key = mapM_ char

many :: Par a -> Par [a]
many thing = alts [return [], do x <- thing; xs <- many thing; return (x:xs)]

char :: Char -> Par ()
char x = do _ <- Satisfy (== x); return ()

alts :: [Par a] -> Par a
alts = foldl Alt Fail


-- "Parser combinators need four values to report errors", Andrew Partridge & David Wright

data Par a where
  Ret :: a -> Par a
  Bind :: Par a -> (a -> Par b) -> Par b
  Satisfy :: (Char -> Bool) -> Par Char
  Fail :: Par a
  Alt :: Par a -> Par a -> Par a

instance Functor Par where fmap = liftM
instance Applicative Par where pure = Ret; (<*>) = ap
instance Monad Par where (>>=) = Bind

type Res a = Either [Char] (a,[Char])

data K4 a b = K4 -- Four continuations:
  { eps :: a -> Res b            -- success; *no* input consumed
  , succ :: [Char] -> a -> Res b -- success; input consumed
  , fail :: () -> Res b          -- failure; *no* input consumed
  , err :: [Char] -> Res b       -- failure; input consumed (so an error!)
  }

parseWith :: Par a -> String -> a
parseWith parStart chars  = do

  case (run chars parStart kFinal) of
    Left remains -> error $ "failed to parse at position: " ++ show (len - length remains)
    Right (a,[]) -> a
    Right (_,remains) -> error $ "unparsed input remains at: " ++ show (len - length remains)
  where

    len = length chars

    kFinal = K4 { eps = \a -> Right (a,chars)
                , succ = \chars a -> Right (a,chars)
                , fail = \() -> Left chars
                , err = \chars -> Left chars
                }

    run :: [Char] -> Par a -> K4 a b -> Res b
    run chars par k@K4{eps,succ,fail,err} = case par of

      Ret x -> eps x

      Fail -> fail ()

      Satisfy pred -> do
        case chars of
          [] -> fail ()
          c:chars -> if pred c then succ chars c else fail ()

      Alt p1 p2 -> do
        run chars p1 K4{ eps = \a1 ->
                           run chars p2 K4{ eps = \_ -> eps a1 -- left biased
                                          , succ
                                          , fail = \() -> eps a1
                                          , err
                                          }
                       , succ
                       , fail = \() -> run chars p2 k
                       , err
                       }

      Bind par f -> do
        run chars par K4{ eps = \a -> run chars (f a) k
                        , succ = \chars a ->
                            run chars (f a) K4{ eps = \a -> succ chars a -- consume
                                              , succ
                                              , fail = \() -> err chars -- fail->error
                                              , err
                                              }
                        , fail
                        , err
                        }
