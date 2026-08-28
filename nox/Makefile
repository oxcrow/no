NO=opam exec -- dune exec no

all:
	opam exec -- dune build

run:
	@$(NO)

clean:
	opam exec -- dune clean

bin:
	@$(NO) ../test/wtf/main.no | qbe - | cc -x assembler -

.PHONY: test clean test run
