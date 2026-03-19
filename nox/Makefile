EXE=./zig-out/bin/nox

# Number of functions and statements to generate for stress tests
N ?= 1
M ?= 10

all:
	@echo "# Building in DEBUG mode. (Expect performance to be slow. Extremely slow!)"
	@echo "# To gain maximum performance for either distribution or benchmarking,"
	@echo "# compile the RELEASE mode executable first."
	@echo "#"
	@echo "# Run either one of these commands to compile in RELEASE mode,"
	@echo "#   $$ zig build --release=safe"
	@echo "#   $$ make safe"
	@echo "#"
	@echo "# Alterantively, if you simply want to run the stress tests, then run,"
	@echo "#   $$ make stress"
	@echo "#   $$ make perf"
	@echo "#"
	@echo "# This will compile the code in RELEASE mode, then run tests."
	@echo "~"
	@echo -n "$$ "
	zig build
	@echo "~"
safe:
	@echo "# Building in RELEASE mode. (Expect performance to be fast. Compiles to be slow!)"
	@echo "~"
	zig build --release=safe
	@echo "~"
run:
	zig build run -- build test/wtf/main.no
test:
	zig build test
bench:
	@zig run test/perf/utils/genStressTests.zig -- $(N) $(M)
hype:
	@echo "# Running artificial stress test to analyze performance."
	@echo "# hyperfine will be used to run the tests."
	@echo "~"
	@echo -n "$$ "
	zig build --release=safe
	@echo "$$ hyperfine ..."
	@hyperfine --warmup 5 "./zig-out/bin/nox build test/perf/hide/million.no"
	@echo "~"
perf:
	@echo "# Running artificial stress test to analyze performance."
	@echo "# perf will be used to run the tests."
	@echo "#"
	@echo "# In case perf refuses to run due to security reasons, enable it,"
	@echo "#   $$ sudo sysctl -w kernel.perf_event_paranoid=1"
	@echo "#   $$ sudo sysctl -w kernel.kptr_restrict=0"
	@echo "#"
	@echo "# Only the first command is required for running perf stat."
	@echo "# The second is required for perf record."
	@echo "~"
	@echo -n "$$ "
	zig build --release=safe
	@echo "$$ perf stat ..."
	@perf stat -d -- ./zig-out/bin/nox build test/perf/hide/million.no
	@echo "~"
clean:
	rm zig-out/ .zig-cache/ -rf

# Aliases to reduce headache of typing the same make commands again, and again
# I have set, alias m="make" in my zsh config, to simplify using make.
# Here are a few aliases to make it even more easier.
# Instead of typing $ make safe hype perf,
# I can do, $ m s h p
s: safe
b: bench
h: hype
p: perf

.PHONY = clean perf safe run
