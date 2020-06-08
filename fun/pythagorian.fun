
(.) f g x = f (g x)

-- TODO: put the nil continuation behnd a thunk (we are strict!) does this improve performance?

nil n c = n
cons x xs n c = c x xs

singleton x = cons x nil

filter f = fix \loop xs. xs nil \x xs. if f x then cons x (loop xs) else loop xs
map f = fix \loop xs. xs nil \x xs. cons (f x) (loop xs)
foldl f = fix \loop acc xs. xs acc \x xs. loop (f acc x) xs

(++) = fix \(++) xs ys. xs ys \x xs. cons x (xs ++ ys)
(>>) xs f = (fix \loop xs. xs nil \x xs. f x ++ (loop xs)) xs

upto a b = (fix \loop a. if b<a then nil else cons a (loop (a+1))) a

triple x y z = \c. c x y z

sqr x = x*x
isPythagorian t = t \x y z. sqr x + sqr y == sqr z

cands n = upto 1 n >> \x. upto x n >> \y. upto y n >> \z. singleton (triple x y z)

showTrip t = t \a b c. "(" ^ showInt a ^ "," ^ showInt b ^ "," ^ showInt c ^ ")"
unwords = foldl (^) ""

prog = unwords . map showTrip . filter isPythagorian . cands

main = prog 20
