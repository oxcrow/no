all:
	# Generate the parser
	flex lexer.l
	bison -Wall -Wcounterexamples -Wother -Werror -d parser.y
	# Compile the grammar
	gcc -c parser.tab.c
	gcc -c lex.yy.c
	gcc -c main.c
	# Link the executable
	gcc -o ng main.o parser.tab.o lex.yy.o
run:
	@echo -n "wtf/main.no: "
	@./ng ../wtf/main.no
	@echo -n "      oh.no: "
	@./ng oh.no
clean:
	-rm *.tab.h *.tab.c *.yy.c *.o ng

.PHONY: clean run
