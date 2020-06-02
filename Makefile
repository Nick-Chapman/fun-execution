
top: regression.diffs

regression.diffs: test.expected test.out
	diff $^

EXES = exe/combinator-fact.exe exe/fact.exe exe/list-processing.exe exe/nfib.exe exe/nthPrime.exe exe/over.exe exe/pap-over-app.exe exe/pythagorian.exe exe/thrice-thrice.exe exe/triangle.exe

test.out: test.sh $(EXES)
	./test.sh > $@

.PRECIOUS: gen/%.c obj/%.o exe/%.exe

exe/%.exe: bc/engine.o obj/%.o
	mkdir -p exe; gcc $^ -o $@

obj/%.o: gen/%.c bc/value.h
	mkdir -p obj; gcc -Wall -Werror -c $< -o $@

gen/%.c: fun/%.fun src/*.hs
	mkdir -p gen; stack run -- $(patsubst gen/%.c,%,$@)

bc/engine.o: bc/engine.c bc/value.h
	gcc -Wall -Werror -c $< -o $@
