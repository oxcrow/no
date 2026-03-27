PASS=opam exec -- dune exec no

all:
	opam exec -- dune build
run:
	@opam exec -- dune exec no test/wtf/main.no
clean:
	opam exec -- dune clean
test:
	${PASS} test/wtf/main.no

.PHONY: test clean test run
