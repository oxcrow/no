PASS=opam exec -- dune exec nxn

all:
	opam exec -- dune build
run:
	@opam exec -- dune exec nxn test/wth/main.nxn
clean:
	opam exec -- dune clean
test:
	${PASS} test/wth/main.nxn
	${PASS} test/norm/001-simple/001-return/pass/001A.nxn
	${PASS} test/norm/001-simple/001-return/pass/002A.nxn
	${PASS} test/norm/001-simple/002-variable/pass/001A.nxn
	${PASS} test/norm/001-simple/002-variable/pass/002A.nxn
	${PASS} test/norm/001-simple/002-variable/pass/003A.nxn

.PHONY: test
