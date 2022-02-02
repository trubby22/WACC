# Sample Makefile for the WACC Compiler lab: edit this to build your own comiler

# Useful locations

ANTLR_DIR_1 := src/antlr
ANTLR_DIR_2 := src/main/antlr4/ic/doc/group15/antlr/.antlr
OUTPUT_DIR	:= target

# Project tools

MVN := mvn
RM	:= rm -rf

# The make rules:

# using only 'mvn package' doesn't work because then maven complains that it couldn't find class ic.doc.group15.MainKt
all:
	$(MVN) compile
	$(MVN) package

test:
	$(MVN) test

# clean up all of the compiled files
# there's a bug that causes you to have to run 'make clean' twice to remove all folders
clean:
	$(RM) $(ANTLR_DIR_1) $(ANTLR_DIR_2) $(OUTPUT_DIR)

.PHONY: all test clean
