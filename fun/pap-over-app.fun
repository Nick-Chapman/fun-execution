
-- This example is designed to stress the engine's support for pap/over-app
-- But to do this, it needs to be compiled *without* NBE

id x = x

f2 a b = a+b
f3 a b c = a+b+c
f4 a b c d = a+b+c+d
f5 a b c d e = a+b+c+d+e

g2 = id (f2 1) 2                                                                    --3
g3 = id (f3 1) 2 3     + id (f3 1 2) 3                                              --6 + 6
g4 = id (f4 1) 2 3 4   + id (f4 1 2) 3 4   + id (f4 1 2 3) 4                        --10 + 10 + 10
g5 = id (f5 1) 2 3 4 5 + id (f5 1 2) 3 4 5 + id (f5 1 2 3) 4 5 + id (f5 1 2 3 4) 5  --15 + 15 + 15 + 15

main = (g2 + g3 + g4 + g5)


main
