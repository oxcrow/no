NO=./build/no

all:
	@cmake --build build
clean:
	-rm build/* -rf
test:
	-@$(NO) ../test/step/001A/pass/001A.no
	-@$(NO) ../test/step/001A/pass/001B.no
	-@$(NO) ../test/step/001A/pass/001C.no
	-@$(NO) ../test/step/001A/pass/002A.no
	-@$(NO) ../test/step/001A/pass/002B.no
	-@$(NO) ../test/step/001A/pass/002C.no
	-@$(NO) ../test/step/001A/pass/002D.no
run:
	@$(NO) ../test/wtf/main.no

MAKEFLAGS += --no-print-directory
.PHONY: clean test run
