
($) x f = f x

u = 42

nil n u = n u
(:) x xs n c = c x xs

(++) = fix \(++) xs ys. xs (\_.ys) \x xs. x : (xs ++ ys)
bind xs f = (fix \bind_f xs. xs (\_.nil) \x xs. f x ++ bind_f xs) xs
map f = fix \loop xs. xs (\u. nil) \x xs. f x : loop xs
foldl f = fix \loop acc xs. xs (\u. acc) \x xs. loop (f acc x) xs
upto a b = (fix \loop a. if b<a then nil else a : loop (a+1)) a

explode s = map (index s) (upto 0 (size s - 1))
implode = foldl (^) ""

showL xs = xs (\_."[]") \x1 xs. "[" ^ showInt x1 ^ implode (map (\x. "," ^ showInt x) xs) ^ "]"

return x = [x]
(>>=) = bind

--sam = [1,2,3] >>= \x. [4,5] >>= \y -> [6,7,8] >>= \z -> return ((100*x) + (10*y) + z)

sam = do x <- [1,2,3]; y <- [4,5]; z <- [6,7,8]; return ((100*x) + (10*y) + z)

prog = showL sam
prog
