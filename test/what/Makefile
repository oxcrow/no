NG=./ng

# Compile the grammar
all:
	gcc -c parser.tab.c
	gcc -c lex.yy.c
	gcc -c main.c
	# Link the executable
	gcc -o ng main.o parser.tab.o lex.yy.o

# Generate the parser
start:
	flex lexer.l
	bison -Wall -Wcounterexamples -Wother -Werror -d parser.y

clean:
	-rm *.tab.h *.tab.c *.yy.c *.o ng
get:
	sudo apt-get install flex bison --yes

run:
	@$(NG) ../pain/001/AA/main.no

.PHONY: clean run
