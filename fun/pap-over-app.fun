
id x = x

f2 a b = a+b
f3 a b c = a+b+c
f4 a b c d = a+b+c+d
f5 a b c d e = a+b+c+d+e

g2 = id (f2 1) 2
g3 = id (f3 1) 2 3     + id (f3 1 2) 3
g4 = id (f4 1) 2 3 4   + id (f4 1 2) 3 4   + id (f4 1 2 3) 4
g5 = id (f5 1) 2 3 4 5 + id (f5 1 2) 3 4 5 + id (f5 1 2 3) 4 5 + id (f5 1 2 3 4) 5

g2 + g3 + g4 + g5

