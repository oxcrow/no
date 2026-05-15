NO=./no

all:
build:
	@go build
debug:
	@echo "# Building in DEBUG mode. (Expect performance to be SLOW!)"
	@echo "# For distribution or benchmarking, build RELEASE mode."
	@echo "# To aid in debugging and development,"
	@echo "#   + Optimizations are disabled.(-N)"
	@echo "#   + Inlining is disabled.(-l)"
	@echo "# ~"
	@echo -n "$$ "
	go build -gcflags=all="-N -l"
race:
	@echo "# Building in DEBUG (RACE) mode. (Expect performance to be SLOW!)"
	@echo "# Since we enable race condition checks expect SLOW performance!"
	@echo "#   + Expect code to use 10-100x more memory. (due to TSAN)"
	@echo "#   + Expect code to run 2-20x slower."
	@echo "# To aid in debugging and development,"
	@echo "#   + Race condition detection is enabled.(-race)"
	@echo "#   + Optimizations are disabled.(-N)"
	@echo "#   + Inlining is disabled.(-l)"
	@echo "# ~"
	@echo -n "$$ "
	go build -race -gcflags=all="-N -l"
fast:
	@echo "# Building in RELEASE mode. (Expect performance to be FAST!)"
	@echo "# To optimize binary size and improve security,"
	@echo "#   + Remove symbol table and debug information.(-s)"
	@echo "#   + Remove DWARF debugging information.(-w)"
	@echo "#   + Remove local file paths.(-trimpath)"
	@echo "# ~"
	@echo -n "$$ "
	go build -ldflags="-s -w" -trimpath
clean:
	-go clean
test: build
	-@$(NO) ../test/step/001A/pass/001A.no
	-@$(NO) ../test/step/001A/pass/001B.no
	-@$(NO) ../test/step/001A/pass/001C.no
x:
	-@$(NO) ../test/step/001A/pass/002A.no
	-@$(NO) ../test/step/001A/pass/002B.no
	-@$(NO) ../test/step/001A/pass/002C.no
	-@$(NO) ../test/step/001A/pass/002D.no
run:
	@#go run . -- ../test/step/001A/pass/001A.no
	@go run . -- ../test/wtf/main.no

MAKEFLAGS += --no-print-directory
.PHONY: build clean test run
