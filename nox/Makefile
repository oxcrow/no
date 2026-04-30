NO=./no

all:
build:
	@go build
clean:
	-go clean
test: build
	-@$(NO) ../test/step/001A/pass/001A.no
	-@$(NO) ../test/step/001A/pass/001B.no
	-@$(NO) ../test/step/001A/pass/001C.no
	-@$(NO) ../test/step/001A/pass/002A.no
	-@$(NO) ../test/step/001A/pass/002B.no
	-@$(NO) ../test/step/001A/pass/002C.no
	-@$(NO) ../test/step/001A/pass/002D.no
run:
	@go run . -- ../test/wtf/main.no

MAKEFLAGS += --no-print-directory
.PHONY: build clean test run
