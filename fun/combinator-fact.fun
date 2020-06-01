
zero z _ = z
succ x _ s = s x

one = succ zero
two = succ one
three = succ two

show = fix \show n. n 0 \p. 1 + show p

add = fix \add a b. a b \a. succ (add a b)
mul = fix \mul a b. a zero \a'. add b (mul a' b)
fact = fix \fact n. n one \p. mul n (fact p)

main = show (fact (fact three))
