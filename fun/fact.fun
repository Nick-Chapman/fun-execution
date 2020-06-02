
fact = fix \fact n. if n == 0 then 1 else n * fact (n - 1)
main = fact (readInt (argv 1))
