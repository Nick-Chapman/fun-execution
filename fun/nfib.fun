
nfib = fix \nfib n. if n < 2 then 1 else nfib (n-1) + nfib (n-2) + 1
main = nfib (readInt (argv 1))
main
