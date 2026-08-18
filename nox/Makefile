NO=./build/dev/no
NX=./build/rel/no

# Build and run our compiler
all:
	@echo "# Building in DEBUG mode. (Expect performance to be slow. Extremely slow!)"
	@echo "# To gain maximum performance for either distribution or benchmarking,"
	@echo "# compile the RELEASE mode executable first."
	@echo "~"
	@if [ ! -d "build/dev" ]; then \
		echo "# Building CMake project.";\
		echo "~";\
		cmake -DMODE=0 -B build/dev;\
	fi
	@cmake --build build/dev

start:
	@echo "# Building CMake project."
	@echo "~"
	cmake -DMODE=0 -B build/dev
done:
	@echo "# Building CMake project in RELEASE mode! (Compilation may be slow!)"
	@echo "# musl is needed to compile static binaries"
	@echo "# Ensure they are installed. (make get)"
	@echo "~"
	CC=musl-gcc cmake -DMODE=1 -B build/rel
	@cmake --build build/rel

strip:
	@echo "# Strip all symbols so the binary remains small."
	@echo "~"
	@echo -n "$$ "
	strip --strip-all $(NX)
clean:
	-rm build/* -rf

get:
	@echo "# We need gcc compilers to build anything"
	@echo "~"
	@echo -n "$$ "
	sudo apt-get install build-essential --yes
	@echo "# We need musl for building static binaries"
	@echo "~"
	@echo -n "$$ "
	sudo apt-get install musl musl-dev musl-tools --yes

test:
	@./build/dev/unit
run:
	@$(NO) compile ../test/pain/001/AA/main.no

MAKEFLAGS += --no-print-directory
.PHONY: clean test run
