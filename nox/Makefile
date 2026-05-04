NO=./build/no

all:
	@cmake --build build
clean:
	rm build/* -rf
run:
	@$(NO) ../test/wtf/main.no

MAKEFLAGS += --no-print-directory
.PHONY: clean run
