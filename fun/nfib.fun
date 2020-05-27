
y f = (\x. f (\v. x x v)) (\x. f (\v. x x v))

nfib = y \nfib n. if n < 2 then 1 else 1 + nfib (n-1) + nfib (n-2)
nfib 10
