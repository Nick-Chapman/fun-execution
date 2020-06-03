
top: regression.diffs

all: $(EXES)

EXAMPLES = combinator-fact fact list-processing nfib nthPrime over pap-over-app pythagorian thrice-thrice triangle
EXES = $(patsubst %, $(OUT)/%, $(EXAMPLES))
BC = bc

OUT = _build

.PRECIOUS: $(OUT)/%
.SECONDARY:

regression.diffs: test/test.expected $(OUT)/test.out
	diff $^

$(OUT)/test.out: test/test.sh $(EXES)
	test/test.sh > $@

$(OUT)/%: $(OUT)/main.o $(OUT)/engine.o $(OUT)/%.o
	gcc $^ -o $@

$(OUT)/%.o: $(OUT)/%.c $(BC)/value.h .dir
	gcc -I$(BC) -Wall -Werror -c $< -o $@

$(OUT)/%.c: fun/%.fun src/*.hs .dir
	stack run batch $< $@

$(OUT)/engine.o: $(BC)/engine.c $(BC)/value.h .dir
	gcc -Wall -Werror -c $< -o $@

$(OUT)/main.o: $(BC)/main.c $(BC)/value.h .dir
	gcc -Wall -Werror -c $< -o $@

.dir:
	mkdir -p $(OUT)
