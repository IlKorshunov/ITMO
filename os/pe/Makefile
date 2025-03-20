CC=g++
CFLAGS=-c -Wall

all: pe-parser

validation-pe-tests: all
	python3 -m tests ValidatingPeTestCases -f

import-dll-tests: all
	python3 -m tests ImportDllTestCases -f

import-function-tests: all
	python3 -m tests ImportFunctionTestCases -f

pe-parser: pe-parser.o
	$(CC) pe-parser.o -o pe-parser

pe-parser.o: pe-parser.cpp
	$(CC) $(CFLAGS) pe-parser.cpp

clean:
	rm -rf *.o pe-parser