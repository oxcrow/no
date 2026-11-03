NO=OPAMROOTISOK=1 opam exec -- dune exec no

all:
	@OPAMROOTISOK=1 opam exec -- dune build

run:
	# 1-stmt-return
	@$(NO) compile ../test/step/1-stmt-return/pass/aa.no
	# 2-stmt-yield
	@$(NO) compile ../test/step/2-stmt-yield/pass/aa.no
	@$(NO) compile ../test/step/2-stmt-yield/pass/ab.no
	@$(NO) compile ../test/step/2-stmt-yield/pass/ac.no
	@$(NO) compile ../test/step/2-stmt-yield/pass/ad.no
	@$(NO) compile ../test/step/2-stmt-yield/pass/ae.no
	@$(NO) compile ../test/step/2-stmt-yield/pass/af.no
	# 3-expr-comment
	@$(NO) compile ../test/step/3-expr-comment/pass/aa.no
	# 4-expr-binary
	@$(NO) compile ../test/step/4-expr-binary/pass/aa.no
	@$(NO) compile ../test/step/4-expr-binary/pass/ab.no
	@$(NO) compile ../test/step/4-expr-binary/pass/ac.no

clean:
	opam exec -- dune clean

bin:
	@$(NO) ../test/wtf/main.no | qbe - | cc -x assembler -

.PHONY: test clean test run
