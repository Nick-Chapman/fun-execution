
top: regression.diffs regression-nn.diffs

all: $(EXES)

# use (slower) FOS for regression testing
FOS = -fos

EXAMPLES = combinator-fact fact list-processing nfib nthPrime over pap-over-app pythagorian thrice-thrice triangle parser do-example json-parser
EXES = $(patsubst %, $(OUT)/%, $(EXAMPLES))
EXES_NN = $(patsubst %, $(OUT)/%-nn, $(EXAMPLES))
BC = bc

OUT = _build


DEBUG = -g

.PRECIOUS: $(OUT)/%
.SECONDARY:

regression.diffs: test/liberal-diff.sh test/test.expected $(OUT)/test.out
	$^

regression-nn.diffs: test/liberal-diff.sh test/test-nn.expected $(OUT)/test-nn.out
	$^

$(OUT)/test.out: test/test.sh $(EXES)
	test/test.sh > $@

$(OUT)/test-nn.out: test/test.sh $(EXES_NN)
	test/test.sh -nn > $@

$(OUT)/%: $(OUT)/%-$(FOS) Makefile
	cp $< $@

$(OUT)/%-nn: $(OUT)/%-$(FOS)-nn Makefile
	cp $< $@

$(OUT)/%-$(FOS): $(OUT)/main.o $(OUT)/engine.o $(OUT)/%-$(FOS).o
	gcc $^ -o $@

$(OUT)/%-$(FOS)-nn: $(OUT)/main.o $(OUT)/engine.o $(OUT)/%-$(FOS)-nn.o
	gcc $^ -o $@

$(OUT)/%-$(FOS).o: $(OUT)/%-$(FOS).c $(BC)/value.h .dir
	gcc -I$(BC) -Wall -Werror $(DEBUG) -c $< -o $@

$(OUT)/%-$(FOS)-nn.o: $(OUT)/%-$(FOS)-nn.c $(BC)/value.h .dir
	gcc -I$(BC) -Wall -Werror $(DEBUG) -c $< -o $@

$(OUT)/%-$(FOS).c: fun/%.fun src/*.hs .dir
	stack run batch -- $(FOS) $< $@

$(OUT)/%-$(FOS)-nn.c: fun/%.fun src/*.hs .dir
	stack run batch -- -nn $(FOS) $< $@

$(OUT)/engine.o: $(BC)/engine.c $(BC)/value.h .dir
	gcc -Wall -Werror $(DEBUG) -c $< -o $@

$(OUT)/main.o: $(BC)/main.c $(BC)/value.h .dir
	gcc -Wall -Werror $(DEBUG) -c $< -o $@

.dir:
	mkdir -p $(OUT)
