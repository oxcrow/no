NO=opam exec -- dune exec no

all:
	opam exec -- dune build

run:
	@#$(NO) compile ../test/step/001/AA/AA.no
	@#$(NO) compile ../test/step/001/AA/AB.no
	@#$(NO) compile ../test/step/001/AA/AC.no
	@#$(NO) compile ../test/step/001/AB/AA.no
	@#$(NO) compile ../test/step/001/AB/AB.no
	@#$(NO) compile ../test/step/001/AB/AC.no
	@#$(NO) compile ../test/step/001/AC/AA.no
	@#$(NO) compile ../test/step/001/AC/AB.no
	@$(NO) compile ../test/step/002/AA/AA.no
	@#$(NO) compile ../test/step/002/AA/AB.no

clean:
	opam exec -- dune clean

bin:
	@$(NO) ../test/wtf/main.no | qbe - | cc -x assembler -

.PHONY: test clean test run
