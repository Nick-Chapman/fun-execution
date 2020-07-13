
(.) f g x = f (g x)

u = 99 -- unit

pair x y f = f x y

nil       = \n c. n u
cons x xs = \n c. c x xs

(:) = cons

map f = fix \loop xs. xs (\u. nil) \x xs. f x : loop xs
foldl f = fix \loop acc xs. xs (\u. acc) \x xs. loop (f acc x) xs
upto a b = (fix \loop a. if b<a then nil else a : loop (a+1)) a
(++) = fix \(++) xs ys. xs ys \x xs. x : (xs ++ ys)
reverse = foldl (\xs x. x:xs) nil

explode s = map (index s) (upto 0 (size s - 1))
implode cs = foldl (^) "" (map showChar cs)

not x = if x then false else true
(>) a b = b < a
(<=) a b = not (a > b)
(>=) a b = not (a < b)
(&&) a b = if a then b else false


elem y = fix \elem_y xs. xs (\_. false) \x xs. if x == y then true else elem_y xs
notElem y =  not . elem y


-- JSON AST
Jobject x  = \o a s i t f n. o x
Jarray x   = \o a s i t f n. a x
Jstring x  = \o a s i t f n. s x
Jinteger x = \o a s i t f n. i x
Jtrue      = \o a s i t f n. t u
Jfalse     = \o a s i t f n. f u
Jnull      = \o a s i t f n. n u


-- AST printer
dq = showChar '"' --"
showString s = dq ^ s ^ dq
cMember chunks acc pair = pair \k v. chunks (": " : (showString k : acc)) v
cObject chunks xs acc = xs (\u. "{}" : acc) (\x xs. " }" : foldl (\acc x -> cMember chunks (", " : acc) x) (cMember chunks ("{ " : acc) x) xs)
cArray chunks xs acc = xs (\u. "[]" : acc) (\x xs. " ]" : foldl (\acc x -> chunks (", " : acc) x) (chunks ("[ " : acc) x) xs)
cString s acc = showString s : acc
cInteger n acc = showInt n : acc
cTrue u acc = "true" : acc
cFalse u acc = "false" : acc
cNull u acc = "null" : acc
chunks = fix \chunks acc j. (j (cObject chunks) (cArray chunks) cString cInteger cTrue cFalse cNull) acc
pretty j = foldl (^) "" (reverse (chunks nil j))


-- Build JSON AST for arithmetic expressions
jlit n = Jobject [pair "lit" (Jinteger n)] -- no extra info
jadd x y = Jobject [pair "add" (Jarray [x,y])] --no isAdd
jnfib = fix \jnfib n. if n < 2 then jlit 1 else jadd (jlit 1) (jadd (jnfib (n-1)) (jnfib (n-2)))


-- Eval JSON carrying arith expressions
getSingle xs g = xs (\_. error "getSingle[]") (\x xs. xs (\_.g x) (\_ _. error "getSingle:>=2"))
getList2 xs g = xs (\_. error "getList2[]") (\x1 xs. getSingle xs \x2. g x1 x2)
ee tag _ = error ("getObject: " ^ tag)
getObject j g = j g (ee"A")(ee"S")(ee"I")(ee"T")(ee"F")(ee"N")
ee tag _ = error ("getArray: " ^ tag)
--getArray j g = j (ee"O")g(ee"S")(ee"I")(ee"T")(ee"F")(ee"N") -- PARSE ERROR - FIX
getArray j g = j (ee"O") g (ee"S")(ee"I")(ee"T")(ee"F")(ee"N")
ee tag _ = error ("getInteger: " ^ tag)
getInteger j = j (ee"O")(ee"A")(ee"S") (\n.n) (ee"T")(ee"F")(ee"N")
getArr2 j g = getArray j \js. getList2 js g
eval = fix \eval j. getObject j \ms. getSingle ms \m. m \k v. if eqString k "lit" then getInteger v else getArr2 v \x y. eval x + eval y




--parse combinators...

return a  = \i r s f e. r a
fail      = \i r s f e. f u
satisfy p = \i r s f e. i f \c i. if p c then s i c else f u
alt p q   = \i r s f e. p i (\a. q i (\b. r a) s (\u. r a) e) s (\u. q i r s f e) e
(>>=) p g = \i r s f e. p i (\a. g a i r s f e) (\i a. g a i (\a. s i a) s (\u. e i) e) f e
fixP g = fix \fixPg. \i r s f e. (g fixPg) i r s f e

showInput i = foldl (^) "" (map showChar i) -- better, show the chr number
finalSucc = \i a. i (\u. a) (\x xs. error ("unconsumed input: " ^ showInput i))
finalErr  = \i  . error ("parse error at: " ^ showInput i)
run p i = p i (finalSucc i) finalSucc (\u. finalErr i) finalErr
parseWith p s = run p (explode s)


mapP par f = do x <- par; return (f x)
(<$>) f par = mapP par f


-- TODO: JSON parser

ret = return
alts = foldl alt fail
many thing = fixP \many_thing. alts [ return [], do x <- thing; xs <- many_thing; return (x:xs)]
char x = satisfy ((==) x) >>=\_. return u


newline = 'N' --error "newline"
tab = 'T' --error "tab"
--read xs = xs -- error "read" -- String->Int

ws = fixP \ws -> alts (ret u : ((char ' '>>=\_. ws) : ((char newline>>=\_. ws) : ((char tab>>=\_. ws) : nil))))

commaSeparated thing = fix \it. thing>>=(\x. alts (ret nil : ((char ','>>=\_. (ws>>=\_. it)) : nil)) >>=(\xs. ret (x:xs)))


digit = satisfy (\c -> (c >= '0') && (c <= '9'))
onenine = satisfy (\c -> (c >= '1') && (c <= '9'))

makeInt = foldl (\acc c -> (10*acc) + (c - '0')) 0

positive = alts [ (do char '0'; return 0) , (do d <- onenine; ds <- many digit; return (makeInt (d : ds))) ]


negate x = 0-x

integer = alts [ positive, do char '-'; (negate <$> positive)]
number = Jinteger <$> integer


character = satisfy (\c -> (c >= ' ') && notElem c ['"','\']) --"
characters = many character

str = do char '"'; cs <- characters; char '"'; return (implode cs)
string = Jstring <$> str


mapM_ f = fix \mapM_f xs. xs (\_. return u) (\x xs. do f x; mapM_f xs)

keyword s = mapM_ char (explode s)

ktrue = do keyword "true"; return Jtrue
kfalse = do keyword "false"; return Jfalse
knull = do keyword "null"; return  Jnull


member e0 = do s <- str; ws; char ':'; ws; v <- e0; return (pair s v)
members e0 = commaSeparated (member e0)
object e0 = do char '{'; ws; ms <- alts [return [], members e0]; char '}'; return (Jobject ms)
elements e0 = commaSeparated e0
array e0 = do char '['; ws; es <- alts [return [], elements e0]; char ']'; return (Jarray es)
value e0 = alts [object e0,array e0,string,number,ktrue,kfalse,knull]
recursiveStructure e0 = do v <- value e0; ws; return v
json = do ws; fixP (\element -> recursiveStructure element)

parse = parseWith json

pipeline n = let ast0 = jnfib n in let res0 = eval ast0 in let str = pretty ast0 in let len = size str in let ast1 = parse str in let res1 = eval ast1 in if not (res1 == res0) then error "res1 <> res0" else len

main = pipeline (readInt (argv 1))
