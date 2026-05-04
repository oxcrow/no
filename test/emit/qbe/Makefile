SOURCES = $(wildcard *.q)
OBJECTS = $(SOURCES:.q=.s)
EXECUTABLES = $(OBJECTS:.s=.out)

all: $(EXECUTABLES)
%.out: %.s
	cc $< -o $@
%.s: %.q
	qbe $< -o $@
run:
	@./test.sh
clean:
	-rm *.s *.out

.PHONY: all run clean
