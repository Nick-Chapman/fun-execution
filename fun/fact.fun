
y f = (\x. f (\v. x x v)) (\x. f (\v. x x v))

fact = y \fact n. if n == 0 then 1 else n * fact (n - 1)

fact 6
