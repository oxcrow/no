BUILD=opam exec -- dune exec no build

all:
	opam exec -- dune build
bin:
	@$(BUILD) test/wtf/main.no -- --emit-qbe | qbe -o -| cc -x assembler - -o a.out
run:
	@${BUILD} test/wtf/main.no --
clean:
	opam exec -- dune clean
	-rm *.out
test:
	${BUILD} test/wtf/main.no

.PHONY: test clean test run bin
