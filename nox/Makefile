NO=./build/dev/no

# Build and run our solvers
all:
	@echo "# Building in DEBUG mode. (Expect performance to be slow. Extremely slow!)"
	@echo "# To gain maximum performance for either distribution or benchmarking,"
	@echo "# compile the RELEASE mode executable first."
	@echo "~"
	@cmake --build build/dev
start:
	@echo "# Building CMake project."
	@echo "~"
	cmake -B build/dev
clean:
	-rm build/* -rf
run:
	@$(NO)

strip:
	@echo "# Strip all symbols so the binary remains small."
	@echo "~"
	@echo -n "$$ "
	strip --strip-all $(NO)

MAKEFLAGS += --no-print-directory
.PHONY: clean test run
