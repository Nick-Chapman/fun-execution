
top: perf-inter

BUILD = _build

perf-inter: $(BUILD)/inter-2.exe
	./$<

perf-native: $(BUILD)/native-2.exe
	./$<


$(BUILD)/native-%.exe: $(BUILD)/native-%.o
	gcc $^ -o $@

$(BUILD)/native-%.o: native.c
	gcc -O$(patsubst $(BUILD)/native-%.o,%,$@) -Wall -Werror -c $< -o $@


$(BUILD)/inter-%.exe: $(BUILD)/inter-%.o $(BUILD)/engine-%.o $(BUILD)/bytecode-%.o
	gcc $^ -o $@

BC = ../bc

$(BUILD)/inter-%.o: inter.c
	gcc -I$(BC) -O$(patsubst $(BUILD)/inter-%.o,%,$@) -Wall -Werror -c $< -o $@

$(BUILD)/bytecode-%.o: ../_build/c/nfib.c $(BC)/value.h
	gcc -I$(BC) -O$(patsubst $(BUILD)/bytecode-%.o,%,$@) -Wall -Werror -c $< -o $@

$(BUILD)/engine-%.o: $(BC)/engine.c $(BC)/value.h
	gcc -O$(patsubst $(BUILD)/engine-%.o,%,$@) -Wall -Werror -c $< -o $@
