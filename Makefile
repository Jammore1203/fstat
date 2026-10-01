FC      = gfortran
FFLAGS ?= -O2 -std=f2018 -Wall -Wextra -Jbuild -Ibuild

.PHONY: all test clean

all: build/fstat

build/stats.o: src/stats.f90
	@mkdir -p build
	$(FC) $(FFLAGS) -c $< -o $@

build/fstat: src/fstat.f90 build/stats.o
	$(FC) $(FFLAGS) $^ -o $@

build/test_stats: tests/test_stats.f90 build/stats.o
	$(FC) $(FFLAGS) $^ -o $@

test: build/test_stats build/fstat
	./build/test_stats
	./build/fstat -c 3 -b 5 tests/latency.csv > build/out.txt
	diff -u tests/expected.txt build/out.txt
	seq 1 100 | ./build/fstat -q | grep -q '^median    50.50'
	@echo "CLI tests passed."

install: build/fstat
	install -m 755 build/fstat $(or $(PREFIX),/usr/local)/bin/fstat

clean:
	rm -rf build
