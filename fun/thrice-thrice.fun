decrement x = x-1
thrice f x = f (f (f x))
prog = thrice thrice decrement
main = prog 100
main
