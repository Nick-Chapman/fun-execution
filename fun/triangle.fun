
y f = (\x. f (\v. x x v)) (\x. f (\v. x x v))

-- tail recursive version

triangle = y (\triangle acc n. if n == 0 then acc else triangle (acc + n) (n - 1)) 0

triangle 100
