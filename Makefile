
all: run/combinator-fact.out run/fact.out run/list-processing.out run/nfib.out run/nthPrime.out run/over.out run/pap-over-app.out run/pythagorian.out run/thrice-thrice.out run/triangle.out

.PRECIOUS: gen/%.c obj/%.o exe/%.exe

run/%.out: exe/%.exe
	mkdir -p run; $< | tee $@

exe/%.exe: bc/engine.o obj/%.o
	mkdir -p exe; gcc $^ -o $@

obj/%.o: gen/%.c bc/value.h
	mkdir -p obj; gcc -Wall -Werror -c $< -o $@

gen/%.c: fun/%.fun src/*.hs
	mkdir -p gen; stack run -- $(patsubst gen/%.c,%,$@)

bc/engine.o: bc/engine.c bc/value.h
	gcc -Wall -Werror -c $< -o $@
