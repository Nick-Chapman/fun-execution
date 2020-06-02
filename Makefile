
top: regression.diffs

all: $(EXES)

EXAMPLES = combinator-fact fact list-processing nfib nthPrime over pap-over-app pythagorian thrice-thrice triangle
BC = bc
BUILD = _build
EXES = $(patsubst %, $(BUILD)/exe/%, $(EXAMPLES))

.PRECIOUS: $(BUILD)/c/%.c $(BUILD)/obj/%.o $(BUILD)/exe/%

regression.diffs: test/test.expected $(BUILD)/test.out
	diff $^

$(BUILD)/test.out: test/test.sh $(EXES)
	test/test.sh > $@

$(BUILD)/exe/%: $(BUILD)/engine.o $(BUILD)/obj/%.o
	mkdir -p $(BUILD)/exe; gcc $^ -o $@

$(BUILD)/obj/%.o: $(BUILD)/c/%.c $(BC)/value.h
	mkdir -p $(BUILD)/obj; gcc -I$(BC) -Wall -Werror -c $< -o $@

$(BUILD)/c/%.c: fun/%.fun src/*.hs
	mkdir -p $(BUILD)/c; stack run -- $(patsubst $(BUILD)/c/%.c,%,$@)

$(BUILD)/engine.o: $(BC)/engine.c $(BC)/value.h
	mkdir -p $(BUILD); gcc -Wall -Werror -c $< -o $@
