no=./no

all:
build:
	@echo "# Building in DEBUG mode. (Expect performance to be SLOW!)"
	@echo "# For distribution or benchmarking, build RELEASE mode."
	@echo "# To aid in debugging and development,"
	@echo "#   + Optimizations are disabled.(-N)"
	@echo "#   + Inlining is disabled.(-l)"
	@echo "# ~"
	@echo -n "$$ "
	go build -gcflags="./...=-N -l" .

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
	@# We need to build and zip executables for different OS/ARCH.
	@# Here are a few we need,
	@# - linux/amd64 : Standard linux desktops and servers on Intel/AMD CPUs
	@# - linux/arm64 : Raspberry pi, Linux on MAC, AWS on ARM CPUs
	@# - darwin/arm64 : Modern MAC systems that use M1/M2/M3/M4 CPUs
	@# - darwin/amd64 : Oldern MAC systems that use Intel CPUs
	@# - windows/amd64 : Modern Windows systems on Intel/AMD CPUs
	@# - windows/arm64 : Windows systems on ARM CPUs
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
	@# 1. Build the executable before we begin the tests
	@# 2. Search through ../test/step for .no files
	@# 3. Sort the results so they are sorted in asending order
	@# 4. Run each command so that we can test them one by one
	-@find ../test/step/pass/ -maxdepth 2 -type f -name "*.no" -print0 | sort -z | xargs -0 -I {} $(no) compile "{}"

run:
	@go run -gcflags="./...=-N -l" . -- compile wtf.no

MAKEFLAGS += --no-print-directory
.PHONY: build clean test run
