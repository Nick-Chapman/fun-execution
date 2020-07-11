
error s = ("error: " ^ s)

u = 99 -- unit

nil       = \n c. n u
cons x xs = \n c. c x xs
map f = fix \loop xs. xs (\u. nil) \x xs. cons (f x) (loop xs)
foldl f = fix \loop acc xs. xs (\u. acc) \x xs. loop (f acc x) xs
upto a b = (fix \loop a. if b<a then nil else cons a (loop (a+1))) a

explode s = map (index s) (upto 0 (size s - 1))

ret a     = \i r s f e. r a
fail      = \i r s f e. f u
satisfy p = \i r s f e. i f \c i. if p c then s i c else f u
alt p q   = \i r s f e. p i (\a. q i (\b. r a) s (\u. r a) e) s (\u. q i r s f e) e
(>>) p g  = \i r s f e. p i (\a. g a i r s f e) (\i a. g a i (\a. s i a) s (\u. e i) e) f e

fixP g = fix \fixPg. \i r s f e. (g fixPg) i r s f e

showInput i = foldl (^) "" (map showChar i)

finalSucc = \i a. i (\u. a) (\x xs. error ("unconsumed input: " ^ showInput i))
finalErr  = \i  . error ("parse error at: " ^ showInput i)
run p i = p i (finalSucc i) finalSucc (\u. finalErr i) finalErr

tok c = satisfy ((==) c)
lit c n = tok c >>\_. ret n

p_lp = tok '('
p_rp = tok ')'

p_1 = lit '1' 1
p_2 = lit '2' 2
p_3 = lit '3' 3
p_4 = lit '4' 4
p_5 = lit '5' 5
p_6 = lit '6' 6


digit = alt p_1 (alt p_2 (alt p_3 (alt p_4 (alt p_5 p_6))))
number = digit >>(fix\number acc. alt (digit>>\x. number ((10*acc)+x)) (ret acc))
paren thing = p_lp >>\_. thing >>\x. p_rp >>\_. ret x
atom exp = alt number (paren exp)

p_add = lit '+' (+)
p_mul = lit '*' (*)
binop = alt p_add p_mul

exp = fixP \exp. atom exp >>\x1. alt (ret x1) (binop>>\f. exp>>\x2. ret (f x1 x2))

parse s = run exp (explode s)
main = parse (argv 1)
