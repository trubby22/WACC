# Sample Makefile for the WACC Compiler lab: edit this to build your own comiler

# Useful locations

# Project tools

MVN := mvn

# The make rules:

all:
	$(MVN) package -DskipTests

test:
	$(MVN) test

# clean up all of the compiled files
clean:
	$(MVN) clean

.PHONY: all test clean
