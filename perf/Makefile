
# TODO: fix it so the build rules for different examples can co-exist
# need to use the % for the example name, and forget about dfferent opt levels. just keep 3

EX = json-parser
#EX = nfib

top: perf-$(EX)-inter-3

BC = ../bc
FUN = ../fun

OUT = ../_build/perf

.PRECIOUS: $(OUT)/%
.SECONDARY:

perf-$(EX)-inter-%: $(OUT)/$(EX)-inter-%.exe
	./$<

perf-$(EX)-native-%: $(OUT)/$(EX)-native-%.exe
	./$<

$(OUT)/$(EX)-native-%.exe: $(OUT)/$(EX)-native-%.o
	gcc $^ -o $@

$(OUT)/$(EX)-native-%.o: $(EX)-native.c .dir
	gcc -O$(patsubst $(OUT)/$(EX)-native-%.o,%,$@) -Wall -Werror -c $< -o $@

$(OUT)/$(EX)-inter-%.exe: $(OUT)/$(EX)-inter-%.o $(OUT)/engine-%.o $(OUT)/$(EX)-%.o
	gcc $^ -o $@

$(OUT)/$(EX)-inter-%.o: $(EX)-inter.c .dir
	gcc -I$(BC) -O$(patsubst $(OUT)/$(EX)-inter-%.o,%,$@) -Wall -Werror -c $< -o $@

$(OUT)/engine-%.o: $(BC)/engine.c $(BC)/value.h .dir
	gcc -I$(BC) -O$(patsubst $(OUT)/engine-%.o,%,$@) -DNDEBUG --param large-function-growth=2000 -Winline -Wall -Werror -c $< -o $@

$(OUT)/$(EX)-%.o: $(OUT)/$(EX).c $(BC)/value.h .dir
	gcc -I$(BC) -O$(patsubst $(OUT)/$(EX)-%.o,%,$@) -Wall -Werror -c $< -o $@

MAYBE_FLAG_TO_AVOID_NBE = #-nn

$(OUT)/$(EX).c: $(FUN)/$(EX).fun ../src/*.hs .dir Makefile
	stack run batch -- $(MAYBE_FLAG_TO_AVOID_NBE) $< $@

.dir:
	mkdir -p $(OUT)
