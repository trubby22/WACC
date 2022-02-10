package ic.doc.group15

import ic.doc.group15.antlr.BasicLexer
import ic.doc.group15.antlr.BasicParser
import ic.doc.group15.semantics.SemanticError
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.junit.jupiter.api.Nested
import org.junit.jupiter.api.assertThrows
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ValueSource

class SemanticTests {
    private val validFolderPath = "wacc_examples/valid"
    private val invalidFolderPath = "wacc_examples/invalid"

    @Nested
    inner class AdvancedValidFiles {
        private val advancedValidFileFolderPath = "$validFolderPath/advanced"

        @ParameterizedTest(name = "check {0} source code is semantically valid")
        @ValueSource(strings = ["binarySortTree", "hashTable", "ticTacToe"])
        fun checkSourceCodeIsSemanticallyValid(fileName: String) {
            isSemanticallyValid("$advancedValidFileFolderPath/$fileName.wacc")
        }
    }

    @Nested
    inner class ArrayValidFiles {
        private val arrayFolderPath = "$validFolderPath/array"

        @ParameterizedTest(name = "check {0} source code is semantically valid")
        @ValueSource(
            strings = ["array", "arrayBasic", "arrayEmpty", "arrayLength", "arrayLookup", "arrayNested",
                "arrayPrint", "arraySimple", "modifyString", "printRef"]
        )
        fun checkSourceCodeIsSemanticallyValid(fileName: String) {
            isSemanticallyValid("$arrayFolderPath/$fileName.wacc")
        }
    }

    @Nested
    inner class BasicValidFiles {
        private val basicFolderPath = "$validFolderPath/basic"

        @Nested
        inner class ExitValidFiles {
            private val exitFolderPath = "$basicFolderPath/exit"

            @ParameterizedTest(name = "check {0} source code is semantically valid")
            @ValueSource(strings = ["exit-1", "exitBasic", "exitBasic2", "exitWrap"])
            fun checkSourceCodeIsSemanticallyValid(fileName: String) {
                isSemanticallyValid("$exitFolderPath/$fileName.wacc")
            }
        }

        @Nested
        inner class SkipValidFiles {
            private val skipFolderPath = "$basicFolderPath/skip"

            @ParameterizedTest(name = "check {0} source code is semantically valid")
            @ValueSource(strings = ["comment", "commentInLine", "skip"])
            fun checkSourceCodeIsSemanticallyValid(fileName: String) {
                isSemanticallyValid("$skipFolderPath/$fileName.wacc")
            }
        }
    }

    @Nested
    inner class ExpressionValidFiles {
        private val expressionsFolderPath = "$validFolderPath/expressions"

        @ParameterizedTest(name = "check {0} source code is semantically valid")
        @ValueSource(
            strings = ["andExpr", "andOverOrExpr", "boolCalc", "boolExpr1", "charComparisonExpr", "divExpr",
                "equalsExpr", "equalsOverAnd", "equalsOverBool", "equalsOverOr", "greaterEqExpr", "greaterExpr", "intCalc",
                "intExpr1", "lessCharExpr", "lessEqExpr", "lessExpr", "longExpr", "longExpr2", "longExpr3", "longSplitExpr",
                "longSplitExpr2", "minusExpr", "minusMinusExpr", "minusNoWhitespaceExpr", "minusPlusExpr", "modExpr",
                "multExpr", "multNoWhitespaceExpr", "negBothDiv", "negBothMod", "negDividendDiv", "negDividendMod",
                "negDivisorDiv", "negDivisorMod", "negExpr", "notequalsExpr", "notExpr", "ordAndchrExpr", "orExpr",
                "plusExpr", "plusMinusExpr", "plusNoWhitespaceExpr", "plusPlusExpr", "sequentialCount", "stringEqualsExpr"]
        )
        fun checkSourceCodeIsSemanticallyValid(fileName: String) {
            isSemanticallyValid("$expressionsFolderPath/$fileName.wacc")
        }
    }

    @Nested
    inner class FunctionValidFiles {
        private val functionsFolderPath = "$validFolderPath/function"

        @Nested
        inner class NestedFunctionValidFiles {
            private val nestedFunctionsFolderPath = "$functionsFolderPath/nested_functions"

            @ParameterizedTest(name = "check {0} source code is semantically valid")
            @ValueSource(
                strings = ["fibonacciFullRec", "fibonacciRecursive", "fixedPointRealArithmetic",
                    "functionConditionalReturn", "mutualRecursion", "printInputTriangle", "printTriangle",
                    "simpleRecursion"]
            )
            fun checkSourceCodeIsSemanticallyValid(fileName: String) {
                isSemanticallyValid("$nestedFunctionsFolderPath/$fileName.wacc")
            }
        }

        @Nested
        inner class SimpleFunctionValidFiles {
            private val simpleFunctionsFolderPath = "$functionsFolderPath/simple_functions"

            @ParameterizedTest(name = "check {0} source code is semantically valid")
            @ValueSource(
                strings = ["asciiTable", "functionDeclaration", "functionDoubleReturn", "functionIfReturns",
                    "functionManyArguments", "functionMultiReturns", "functionReturnPair", "functionSimple",
                    "functionSimpleLoop", "functionUpdateParameter", "incFunction", "negFunction", "sameArgName",
                    "sameArgName2", "sameNameAsVar"]
            )
            fun checkSourceCodeIsSemanticallyValid(fileName: String) {
                isSemanticallyValid("$simpleFunctionsFolderPath/$fileName.wacc")
            }
        }
    }

    @Nested
    inner class IfValidFiles {
        private val ifValidFileFolderPath = "$validFolderPath/if"

        @ParameterizedTest(name = "check {0} source code is semantically valid")
        @ValueSource(strings = ["if1", "if2", "if3", "if4", "if5", "if6", "ifBasic", "ifFalse", "ifTrue", "whitespace"])
        fun checkSourceCodeIsSemanticallyValid(fileName: String) {
            isSemanticallyValid("$ifValidFileFolderPath/$fileName.wacc")
        }
    }

    @Nested
    inner class IOValidFiles {
        private val ioValidFileFolderPath = "$validFolderPath/IO"

        @ParameterizedTest(name = "check {0} source code is semantically valid")
        @ValueSource(strings = ["IOLoop", "IOSequence"])
        fun checkSourceCodeIsSemanticallyValid(fileName: String) {
            isSemanticallyValid("$ioValidFileFolderPath/$fileName.wacc")
        }

        @Nested
        inner class PrintValidFiles {
            private val printValidFileFolderPath = "$ioValidFileFolderPath/print"

            @ParameterizedTest(name = "check {0} source code is semantically valid")
            @ValueSource(
                strings = ["hashInProgram", "multipleStringsAssignment", "print", "print-backspace",
                    "print-carridge-return", "printBool", "printChar", "printCharArray", "printCharAsString", "printEscChar",
                    "printInt", "println"]
            )
            fun checkSourceCodeIsSemanticallyValid(fileName: String) {
                isSemanticallyValid("$printValidFileFolderPath/$fileName.wacc")
            }
        }

        @Nested
        inner class ReadValidFiles {
            private val readValidFileFolderPath = "$ioValidFileFolderPath/read"

            @ParameterizedTest(name = "check {0} source code is semantically valid")
            @ValueSource(
                strings = ["echoBigInt", "echoBigNegInt", "echoChar", "echoInt", "echoNegInt", "echoPuncChar",
                    "read"]
            )
            fun checkSourceCodeIsSemanticallyValid(fileName: String) {
                isSemanticallyValid("$readValidFileFolderPath/$fileName.wacc")
            }
        }
    }

    @Nested
    inner class PairValidFiles {
        private val pairsValidFileFolderPath = "$validFolderPath/pairs"

        @ParameterizedTest(name = "check {0} source code is semantically valid")
        @ValueSource(
            strings = ["checkRefPair", "createPair", "createPair02", "createPair03", "createRefPair", "free",
                "linkedList", "nestedPair", "null", "printNull", "printNullPair", "printPair", "printPairOfNulls", "readPair",
                "writeFst", "writeSnd"]
        )
        fun checkSourceCodeIsSemanticallyValid(fileName: String) {
            isSemanticallyValid("$pairsValidFileFolderPath/$fileName.wacc")
        }
    }

    @Nested
    inner class RuntimeErrorValidFiles {
        private val runtimeErrorValidFileFolderPath = "$validFolderPath/runtimeErr"

        @Nested
        inner class ArrayOutOfBoundsValidFiles {
            private val arrayOutOfBoundsValidFileFolderPath = "$runtimeErrorValidFileFolderPath/arrayOutOfBounds"

            @ParameterizedTest(name = "check {0} source code is semantically valid")
            @ValueSource(strings = ["arrayNegBounds", "arrayOutOfBounds", "arrayOutOfBoundsWrite"])
            fun checkSourceCodeIsSemanticallyValid(fileName: String) {
                isSemanticallyValid("$arrayOutOfBoundsValidFileFolderPath/$fileName.wacc")
            }
        }

        @Nested
        inner class DivideByZeroValidFiles {
            private val divideByZeroValidFileFolderPath = "$runtimeErrorValidFileFolderPath/divideByZero"

            @ParameterizedTest(name = "check {0} source code is semantically valid")
            @ValueSource(strings = ["divideByZero", "divZero", "modByZero"])
            fun checkSourceCodeIsSemanticallyValid(fileName: String) {
                isSemanticallyValid("$divideByZeroValidFileFolderPath/$fileName.wacc")
            }
        }

        @Nested
        inner class IntegerOverflowValidFiles {
            private val integerOverflowValidFileFolderPath = "$runtimeErrorValidFileFolderPath/integerOverflow"

            @ParameterizedTest(name = "check {0} source code is semantically valid")
            @ValueSource(
                strings = ["intJustOverflow", "intmultOverflow", "intnegateOverflow", "intnegateOverflow2",
                    "intnegateOverflow3", "intnegateOverflow4", "intUnderflow", "intWayOverflow"]
            )
            fun checkSourceCodeIsSemanticallyValid(fileName: String) {
                isSemanticallyValid("$integerOverflowValidFileFolderPath/$fileName.wacc")
            }
        }

        @Nested
        inner class NullDereferenceValidFiles {
            private val nullDereferenceValidFileFolderPath = "$runtimeErrorValidFileFolderPath/nullDereference"

            @ParameterizedTest(name = "check {0} source code is semantically valid")
            @ValueSource(strings = ["freeNull", "readNull1", "readNull2", "setNull1", "setNull2", "useNull1", "useNull2"])
            fun checkSourceCodeIsSemanticallyValid(fileName: String) {
                isSemanticallyValid("$nullDereferenceValidFileFolderPath/$fileName.wacc")
            }
        }
    }

    @Nested
    inner class ScopeValidFiles {
        private val scopeValidFileFolderPath = "$validFolderPath/scope"

        @ParameterizedTest(name = "check {0} source code is semantically valid")
        @ValueSource(
            strings = ["ifNested1", "ifNested2", "indentationNotImportant", "intsAndKeywords", "printAllTypes",
                "scope", "scopeBasic", "scopeIfRedefine", "scopeRedefine", "scopeSimpleRedefine", "scopeVars",
                "scopeWhileNested", "scopeWhileRedefine"]
        )
        fun checkSourceCodeIsSemanticallyValid(fileName: String) {
            isSemanticallyValid("$scopeValidFileFolderPath/$fileName.wacc")
        }
    }

    @Nested
    inner class SequenceValidFiles {
        private val sequenceValidFileFolderPath = "$validFolderPath/sequence"

        @ParameterizedTest(name = "check {0} source code is semantically valid")
        @ValueSource(
            strings = ["basicSeq", "basicSeq2", "boolAssignment", "charAssignment", "exitSimple",
                "intAssignment", "intLeadingZeros", "stringAssignment"]
        )
        fun checkSourceCodeIsSemanticallyValid(fileName: String) {
            isSemanticallyValid("$sequenceValidFileFolderPath/$fileName.wacc")
        }
    }

    @Nested
    inner class VariableValidFiles {
        private val variablesValidFileFolderPath = "$validFolderPath/variables"

        @ParameterizedTest(name = "check {0} source code is semantically valid")
        @ValueSource(
            strings = ["_VarNames", "boolDeclaration", "boolDeclaration2", "capCharDeclaration",
                "charDeclaration", "charDeclaration2", "emptyStringDeclaration", "intDeclaration", "longVarNames",
                "manyVariables", "negIntDeclaration", "puncCharDeclaration", "stringDeclaration", "zeroIntDeclaration"]
        )
        fun checkSourceCodeIsSemanticallyValid(fileName: String) {
            isSemanticallyValid("$variablesValidFileFolderPath/$fileName.wacc")
        }
    }

    @Nested
    inner class WhileValidFiles {
        private val whileValidFileFolderPath = "$validFolderPath/while"

        @ParameterizedTest(name = "check {0} source code is semantically valid")
        @ValueSource(
            strings = ["fibonacciFullIt", "fibonacciIterative", "loopCharCondition", "loopIntCondition", "max",
                "min", "rmStyleAdd", "rmStyleAddIO", "whileBasic", "whileBoolFlip", "whileCount", "whileFalse"]
        )
        fun checkSourceCodeIsSemanticallyValid(fileName: String) {
            isSemanticallyValid("$whileValidFileFolderPath/$fileName.wacc")
        }
    }

    @Nested
    inner class SemanticErrorInvalidFiles {
        private val semanticErrorFileFolderPath = "$invalidFolderPath/semanticErr"

        @Nested
        inner class ExitInvalidFiles {
            private val exitInvalidFileFolderPath = "$semanticErrorFileFolderPath/exit"

            @ParameterizedTest(name = "check {0} source code is not semantically valid")
            @ValueSource(strings = ["badCharExit", "exitNonInt", "globalReturn"])
            fun checkSourceCodeIsSemanticallyValid(fileName: String) {
                assertThrows<SemanticError> {
                    isSemanticallyValid("$exitInvalidFileFolderPath/$fileName.wacc")
                }
            }
        }

        @Nested
        inner class ExpressionInvalidFiles {
            private val expressionsInvalidFileFolderPath = "$semanticErrorFileFolderPath/expressions"

            @ParameterizedTest(name = "check {0} source code is not semantically valid")
            @ValueSource(
                strings = ["boolOpTypeErr", "exprTypeErr", "intOpTypeErr", "lessPairExpr", "mixedOpTypeErr",
                    "moreArrExpr", "stringElemErr"]
            )
            fun checkSourceCodeIsSemanticallyValid(fileName: String) {
                assertThrows<SemanticError> {
                    isSemanticallyValid("$expressionsInvalidFileFolderPath/$fileName.wacc")
                }
            }
        }

        @Nested
        inner class FunctionInvalidFiles {
            private val functionInvalidFileFolderPath = "$semanticErrorFileFolderPath/function"

            @ParameterizedTest(name = "check {0} source code is not semantically valid")
            @ValueSource(
                strings = ["functionAssign", "functionBadArgUse", "functionBadCall", "functionBadParam",
                    "functionBadReturn", "functionOverArgs", "functionRedefine", "functionSwapArgs", "functionUnderArgs",
                    "funcVarAccess"]
            )
            fun checkSourceCodeIsSemanticallyValid(fileName: String) {
                assertThrows<SemanticError> {
                    isSemanticallyValid("$functionInvalidFileFolderPath/$fileName.wacc")
                }
            }
        }

        @Nested
        inner class IfInvalidFiles {
            private val ifInvalidFileFolderPath = "$semanticErrorFileFolderPath/if"

            @ParameterizedTest(name = "check {0} source code is not semantically valid")
            @ValueSource(strings = ["ifIntCondition"])
            fun checkSourceCodeIsSemanticallyValid(fileName: String) {
                assertThrows<SemanticError> {
                    isSemanticallyValid("$ifInvalidFileFolderPath/$fileName.wacc")
                }
            }
        }

        @Nested
        inner class IOInvalidFiles {
            private val ioInvalidFileFolderPath = "$semanticErrorFileFolderPath/IO"

            @ParameterizedTest(name = "check {0} source code is not semantically valid")
            @ValueSource(strings = ["readTypeErr"])
            fun checkSourceCodeIsSemanticallyValid(fileName: String) {
                assertThrows<SemanticError> {
                    isSemanticallyValid("$ioInvalidFileFolderPath/$fileName.wacc")
                }
            }
        }

        @Nested
        inner class MultipleInvalidFiles {
            private val multipleInvalidFileFolderPath = "$semanticErrorFileFolderPath/multiple"

            @ParameterizedTest(name = "check {0} source code is not semantically valid")
            @ValueSource(strings = ["funcMess", "ifAndWhileErrs", "messyExpr", "multiCaseSensitivity", "multiTypeErrs"])
            fun checkSourceCodeIsSemanticallyValid(fileName: String) {
                assertThrows<SemanticError> {
                    isSemanticallyValid("$multipleInvalidFileFolderPath/$fileName.wacc")
                }
            }
        }

        @Nested
        inner class PairInvalidFiles {
            private val pairsInvalidFileFolderPath = "$semanticErrorFileFolderPath/pairs"

            @ParameterizedTest(name = "check {0} source code is not semantically valid")
            @ValueSource(strings = ["freeNonPair", "fstNull", "sndNull"])
            fun checkSourceCodeIsSemanticallyValid(fileName: String) {
                assertThrows<SemanticError> {
                    isSemanticallyValid("$pairsInvalidFileFolderPath/$fileName.wacc")
                }
            }
        }

        @Nested
        inner class PrintInvalidFiles {
            private val printInvalidFileFolderPath = "$semanticErrorFileFolderPath/print"

            @ParameterizedTest(name = "check {0} source code is not semantically valid")
            @ValueSource(strings = ["printTypeErr01"])
            fun checkSourceCodeIsSemanticallyValid(fileName: String) {
                assertThrows<SemanticError> {
                    isSemanticallyValid("$printInvalidFileFolderPath/$fileName.wacc")
                }
            }
        }

        @Nested
        inner class ReadInvalidFiles {
            private val readInvalidFileFolderPath = "$semanticErrorFileFolderPath/read"

            @ParameterizedTest(name = "check {0} source code is not semantically valid")
            @ValueSource(strings = ["readTypeErr01"])
            fun checkSourceCodeIsSemanticallyValid(fileName: String) {
                assertThrows<SemanticError> {
                    isSemanticallyValid("$readInvalidFileFolderPath/$fileName.wacc")
                }
            }
        }

        @Nested
        inner class ScopeInvalidFiles {
            private val scopeInvalidFileFolderPath = "$semanticErrorFileFolderPath/scope"

            @ParameterizedTest(name = "check {0} source code is not semantically valid")
            @ValueSource(strings = ["badScopeRedefine"])
            fun checkSourceCodeIsSemanticallyValid(fileName: String) {
                assertThrows<SemanticError> {
                    isSemanticallyValid("$scopeInvalidFileFolderPath/$fileName.wacc")
                }
            }
        }

        @Nested
        inner class VariableInvalidFiles {
            private val variableInvalidFileFolderPath = "$semanticErrorFileFolderPath/variables"

            @ParameterizedTest(name = "check {0} source code is not semantically valid")
            @ValueSource(
                strings = ["basicTypeErr01", "basicTypeErr02", "basicTypeErr03", "basicTypeErr04", "basicTypeErr05",
                    "basicTypeErr06", "basicTypeErr07", "basicTypeErr08", "basicTypeErr09", "basicTypeErr10",
                    "basicTypeErr11", "basicTypeErr12", "caseMatters", "doubleDeclare", "undeclaredScopeVar",
                    "undeclaredVar", "undeclaredVarAccess"]
            )
            fun checkSourceCodeIsSemanticallyValid(fileName: String) {
                assertThrows<SemanticError> {
                    isSemanticallyValid("$variableInvalidFileFolderPath/$fileName.wacc")
                }
            }
        }

        @Nested
        inner class WhileInvalidFiles {
            private val whileInvalidFileFolderPath = "$semanticErrorFileFolderPath/while"

            @ParameterizedTest(name = "check {0} source code is not semantically valid")
            @ValueSource(strings = ["falsErr", "truErr", "whileIntCondition"])
            fun checkSourceCodeIsSemanticallyValid(fileName: String) {
                assertThrows<SemanticError> {
                    isSemanticallyValid("$whileInvalidFileFolderPath/$fileName.wacc")
                }
            }
        }
    }

    /**
     * Generate an AST and walk the tree to perform semantic/type checks.
     *
     * @param path Path to the file to be checked semantically.
     * @exception ic.doc.group15.semantics.SemanticError
     */
    private fun isSemanticallyValid(path: String) {
        val input = CharStreams.fromFileName(path)
        val lexer = BasicLexer(input)
        val tokens = CommonTokenStream(lexer)
        val parser = BasicParser(tokens)

        parser.removeErrorListeners()
        parser.addErrorListener(MyErrorListener())

        val tree = parser.program()
        val visitor = MyVisitor()
        visitor.visit(tree)
    }
}

