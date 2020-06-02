
-- tail recursive version

triangle = (fix \triangle acc n. if n == 0 then acc else triangle (acc + n) (n - 1)) 0

main = triangle (readInt (argv 1))
