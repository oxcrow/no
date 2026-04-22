BUILD=opam exec -- dune exec no

all:
	opam exec -- dune build
bin:
	@$(BUILD) test/wtf/main.no -- --emit-qbe | qbe -o -| cc -x assembler - -o a.out
clean:
	opam exec -- dune clean
	-rm *.out
test:
	-$(BUILD) test/norm/step/000A.no -- --emit-qbe
	-$(BUILD) test/norm/step/001A.no -- --emit-qbe
	-$(BUILD) test/norm/step/001B.no -- --emit-qbe
	-$(BUILD) test/norm/step/001C.no -- --emit-qbe
run:
	@$(BUILD) test/norm/step/001D.no -- --emit-qbe
	@#$(BUILD) test/wtf/main.no

.PHONY: test clean test run bin
