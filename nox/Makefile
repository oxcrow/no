BUILD=opam exec -- dune exec no build

all:
	opam exec -- dune build
run:
	@${BUILD} test/wtf/main.no
clean:
	opam exec -- dune clean
test:
	${BUILD} test/wtf/main.no

.PHONY: test clean test run
