NO=OPAMROOTISOK=1 opam exec -- dune exec no

all:
	@OPAMROOTISOK=1 opam exec -- dune build

run:
	@$(NO) compile ../test/step/1-stmt-return/pass/aa.no
x:
	@$(NO) compile ../test/step/2-stmt-yield/pass/aa.no
	@$(NO) compile ../test/step/2-stmt-yield/pass/ab.no
	@$(NO) compile ../test/step/2-stmt-yield/pass/ac.no
	@$(NO) compile ../test/step/3-expr-comment/pass/aa.no
y:
	@$(NO) compile ../test/step/4-expr-binary/pass/aa.no
	@$(NO) compile ../test/step/4-expr-binary/pass/ab.no
	@$(NO) compile ../test/step/4-expr-binary/pass/ac.no

clean:
	opam exec -- dune clean

bin:
	@$(NO) ../test/wtf/main.no | qbe - | cc -x assembler -

.PHONY: test clean test run
