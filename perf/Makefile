
# TODO: fix it so the build rules for different examples can co-exist
# need to use the % for the example name, and forget about dfferent opt levels. just keep 3

EX = json-parser
#EX = nfib

top: perf-$(EX)-inter

BC = ../bc
FUN = ../fun

OUT = ../_build/perf

DEBUG = -g
OPT = 3

.PRECIOUS: $(OUT)/%
.SECONDARY:

perf-$(EX)-inter: perf-$(EX)-inter-$(OPT)

perf-$(EX)-inter-$(OPT): $(OUT)/$(EX)-inter-$(OPT).exe
	./$<

perf-$(EX)-native-$(OPT): $(OUT)/$(EX)-native-$(OPT).exe
	./$<

$(OUT)/$(EX)-native-$(OPT).exe: $(OUT)/$(EX)-native-$(OPT).o
	gcc $^ -o $@

$(OUT)/$(EX)-native-$(OPT).o: $(EX)-native.c .dir
	gcc -O$(OPT) -Wall -Werror $(DEBUG) -c $< -o $@

$(OUT)/$(EX)-inter-$(OPT).exe: $(OUT)/$(EX)-inter-$(OPT).o $(OUT)/engine-$(OPT).o $(OUT)/$(EX)-$(OPT).o
	gcc $^ -o $@

$(OUT)/$(EX)-inter-$(OPT).o: $(EX)-inter.c .dir
	gcc -I$(BC) -O$(OPT) -Wall -Werror $(DEBUG) -c $< -o $@

$(OUT)/engine-$(OPT).o: $(BC)/engine.c $(BC)/value.h .dir
	gcc -I$(BC) -O$(OPT) -DNDEBUG --param large-function-growth=2000 -Winline -Wall -Werror $(DEBUG) -c $< -o $@

$(OUT)/$(EX)-$(OPT).o: $(OUT)/$(EX).c $(BC)/value.h .dir
	gcc -I$(BC) -O$(OPT) -Wall -Werror $(DEBUG) -c $< -o $@

MAYBE_FLAG_TO_AVOID_NBE = #-nn

$(OUT)/$(EX).c: $(FUN)/$(EX).fun ../src/*.hs .dir Makefile
	stack run batch -- $(MAYBE_FLAG_TO_AVOID_NBE) $< $@

.dir:
	mkdir -p $(OUT)
