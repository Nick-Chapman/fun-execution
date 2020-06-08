
top: regression.diffs regression-nn.diffs

all: $(EXES)

EXAMPLES = combinator-fact fact list-processing nfib nthPrime over pap-over-app pythagorian thrice-thrice triangle parser
EXES = $(patsubst %, $(OUT)/%, $(EXAMPLES))
EXES_NN = $(patsubst %, $(OUT)/%-nn, $(EXAMPLES))
BC = bc

OUT = _build

.PRECIOUS: $(OUT)/%
.SECONDARY:

regression.diffs: test/test.expected $(OUT)/test.out
	diff $^

regression-nn.diffs: test/test-nn.expected $(OUT)/test-nn.out
	diff $^

$(OUT)/test.out: test/test.sh $(EXES)
	test/test.sh > $@

$(OUT)/test-nn.out: test/test.sh $(EXES_NN)
	test/test.sh -nn > $@

$(OUT)/%: $(OUT)/main.o $(OUT)/engine.o $(OUT)/%.o
	gcc $^ -o $@

$(OUT)/%-nn: $(OUT)/main.o $(OUT)/engine.o $(OUT)/%-nn.o
	gcc $^ -o $@

$(OUT)/%.o: $(OUT)/%.c $(BC)/value.h .dir
	gcc -I$(BC) -Wall -Werror -c $< -o $@

$(OUT)/%-nn.o: $(OUT)/%-nn.c $(BC)/value.h .dir
	gcc -I$(BC) -Wall -Werror -c $< -o $@

$(OUT)/%.c: fun/%.fun src/*.hs .dir
	stack run batch $< $@

$(OUT)/%-nn.c: fun/%.fun src/*.hs .dir
	stack run batch -- -nn $< $@

$(OUT)/engine.o: $(BC)/engine.c $(BC)/value.h .dir
	gcc -Wall -Werror -c $< -o $@

$(OUT)/main.o: $(BC)/main.c $(BC)/value.h .dir
	gcc -Wall -Werror -c $< -o $@

.dir:
	mkdir -p $(OUT)
