
top: perf-inter-3

BC = ../bc
FUN = ../fun

OUT = ../_build/perf

.PRECIOUS: $(OUT)/%
.SECONDARY:

perf-inter-%: $(OUT)/inter-%.exe
	./$<

perf-native-%: $(OUT)/native-%.exe
	./$<

$(OUT)/native-%.exe: $(OUT)/native-%.o
	gcc $^ -o $@

$(OUT)/native-%.o: native.c .dir
	gcc -O$(patsubst $(OUT)/native-%.o,%,$@) -Wall -Werror -c $< -o $@

$(OUT)/inter-%.exe: $(OUT)/inter-%.o $(OUT)/engine-%.o $(OUT)/nfib-%.o
	gcc $^ -o $@

$(OUT)/inter-%.o: inter.c .dir
	gcc -I$(BC) -O$(patsubst $(OUT)/inter-%.o,%,$@) -Wall -Werror -c $< -o $@

$(OUT)/engine-%.o: $(BC)/engine.c $(BC)/value.h .dir
	gcc -I$(BC) -O$(patsubst $(OUT)/engine-%.o,%,$@) -Wall -Werror -c $< -o $@

$(OUT)/nfib-%.o: $(OUT)/nfib.c $(BC)/value.h .dir
	gcc -I$(BC) -O$(patsubst $(OUT)/nfib-%.o,%,$@) -Wall -Werror -c $< -o $@

$(OUT)/nfib.c: $(FUN)/nfib.fun ../src/*.hs .dir
	stack run batch $< $@

.dir:
	mkdir -p $(OUT)
