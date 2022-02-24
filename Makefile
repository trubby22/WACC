# Sample Makefile for the WACC Compiler lab: edit this to build your own comiler

# Useful locations

ANTLR_DIR_1 := src/antlr
ANTLR_DIR_2 := src/main/antlr4/ic/doc/group15/antlr/.antlr
GEN_DIR_1   := gen
GEN_DIR_2   := src/main/antlr4/ic/doc/group15/antlr/gen

# Project tools

MVN := mvn
RM	:= rm -rf

# The make rules:

all:
	$(MVN) package -DskipTests

test:
	$(MVN) test

asm:
	rm *.s

# clean up all of the compiled files
clean: asm
	$(MVN) clean
	$(RM) $(ANTLR_DIR_1) $(ANTLR_DIR_2) $(GEN_DIR_1) $(GEN_DIR_2)

.PHONY: all test clean asm
