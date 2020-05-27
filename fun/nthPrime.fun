
not b = if b then false else true
(&&) x y = if x then y else false
(||) x y = if x then true else y

(.) f g x = f (g x)

y f = (\x. f (\v. x x v)) (\x. f (\v. x x v))

nil n c = n
cons x xs n c = c x xs

sum = y \sum xs. xs 0 \x xs. x + sum xs
length = y (\length acc xs. xs acc \_ xs. length (acc+1) xs) 0
(++) = y \(++) xs ys. xs ys \x xs. cons x (xs ++ ys)

all f = y \all_f xs. xs true \x xs. f x && all_f xs
divisible a b = a % b == 0

search = y \search n ps i. if all (not . divisible i) ps then (if length ps == (n-1) then i else search n (ps ++ cons i nil) (i+1)) else search n ps (i+1)

nthPrime n = if n < 1 then 0 else if n == 1 then 2 else search n (cons 2 nil) 3

nthPrime 10
