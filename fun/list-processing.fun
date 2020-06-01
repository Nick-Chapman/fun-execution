
(.) f g x = f (g x)

nil n c = n
cons x xs n c = c x xs

map f = fix \map_f xs. xs nil \x xs. cons (f x) (map_f xs)
foldl f = fix \foldl_f acc xs. xs acc \x xs. foldl_f (f acc x) xs
upto a b = (fix \upto_b a. if b<a then nil else cons a (upto_b (a+1))) a

sum = foldl (+) 0

sqr x = x*x
inc x = x+1

pipe = sum . map (inc . sqr) . upto 1

main = pipe 100
