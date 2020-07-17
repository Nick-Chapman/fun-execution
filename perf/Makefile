
top: json # fails with -nn: TODO: why?

json: run-json-parser-inter
nfib: run-nfib-inter

BC = ../bc
FUN = ../fun

OUT = ../_build/perf

#FUN_COMPILE_FLAGS += -nn
FUN_COMPILE_FLAGS += -fos

DEBUG = -g
OPT = 3

NDEBUG_FLAG = -DNDEBUG

.PRECIOUS: $(OUT)/%
.SECONDARY:

run-%-inter: $(OUT)/%-inter-$(OPT).exe
	./$<

run-%-native: $(OUT)/%-native-$(OPT).exe
	./$<

$(OUT)/%-native-$(OPT).exe: $(OUT)/%-native-$(OPT).o
	gcc $^ -o $@

$(OUT)/%-native-$(OPT).o: %-native.c .dir
	gcc -O$(OPT) -Wall -Werror $(DEBUG) -c $< -o $@

$(OUT)/%-inter-$(OPT).exe: $(OUT)/%-inter-$(OPT).o $(OUT)/engine-$(OPT).o $(OUT)/%-$(OPT).o
	gcc $^ -o $@

$(OUT)/%-inter-$(OPT).o: %-inter.c .dir
	gcc -I$(BC) -O$(OPT) -Wall -Werror $(DEBUG) -c $< -o $@

$(OUT)/engine-$(OPT).o: $(BC)/engine.c $(BC)/value.h .dir
	gcc -I$(BC) -O$(OPT) $(NDEBUG_FLAG) --param large-function-growth=2000 -Winline -Wall -Werror $(DEBUG) -c $< -o $@

$(OUT)/%-$(OPT).o: $(OUT)/%.c $(BC)/value.h .dir
	gcc -I$(BC) -O$(OPT) -Wall -Werror $(DEBUG) -c $< -o $@

$(OUT)/%.c: $(FUN)/%.fun ../src/*.hs .dir Makefile
	stack run batch -- $(FUN_COMPILE_FLAGS) $< $@

.dir:
	mkdir -p $(OUT)
