package ic.doc.group15

import org.apache.maven.surefire.shade.org.apache.commons.io.IOUtils
import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Nested
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ValueSource
import java.nio.charset.StandardCharsets

// Verify that parsing works properly
class SyntacticTests {

    enum class ErrorType(val type: String, val code: Int) {
        SYNTAX("syntax", 100),
        SEMANTICS("semantics", 200)
    }

    private val validFolderPath = "wacc_examples/valid"
    private val invalidFolderPath = "wacc_examples/invalid"

    @Nested
    inner class AdvancedValidFiles {
        private val advancedValidFileFolderPath = "$validFolderPath/advanced"

        @ParameterizedTest(name = "check {0} source code is syntactically valid")
        @ValueSource(strings = ["binarySortTree", "hashTable", "ticTacToe"])
        fun checkSourceCodeIsSyntacticallyValid(fileName: String) {
            assertTrue(isSyntaxValid("$advancedValidFileFolderPath/$fileName.wacc"))
        }
    }

    @Nested
    inner class ArrayValidFiles {
        private val arrayFolderPath = "$validFolderPath/array"

        @ParameterizedTest(name = "check {0} source code is syntactically valid")
        @ValueSource(
            strings = ["array", "arrayBasic", "arrayEmpty", "arrayLength", "arrayLookup", "arrayNested",
                "arrayPrint", "arraySimple", "modifyString", "printRef"]
        )
        fun checkSourceCodeIsSyntacticallyValid(fileName: String) {
            assertTrue(isSyntaxValid("$arrayFolderPath/$fileName.wacc"))
        }
    }

    @Nested
    inner class BasicValidFiles {
        private val basicFolderPath = "$validFolderPath/basic"

        @Nested
        inner class ExitValidFiles {
            private val exitFolderPath = "$basicFolderPath/exit"

            @ParameterizedTest(name = "check {0} source code is syntactically valid")
            @ValueSource(strings = ["exit-1", "exitBasic", "exitBasic2", "exitWrap"])
            fun checkSourceCodeIsSyntacticallyValid(fileName: String) {
                assertTrue(isSyntaxValid("$exitFolderPath/$fileName.wacc"))
            }
        }

        @Nested
        inner class SkipValidFiles {
            private val skipFolderPath = "$basicFolderPath/skip"

            @ParameterizedTest(name = "check {0} source code is syntactically valid")
            @ValueSource(strings = ["comment", "commentInLine", "skip"])
            fun checkSourceCodeIsSyntacticallyValid(fileName: String) {
                assertTrue(isSyntaxValid("$skipFolderPath/$fileName.wacc"))
            }
        }
    }

    @Nested
    inner class ExpressionValidFiles {
        private val expressionsFolderPath = "$validFolderPath/expressions"

        @ParameterizedTest(name = "check {0} source code is syntactically valid")
        @ValueSource(
            strings = ["andExpr", "andOverOrExpr", "boolCalc", "boolExpr1", "charComparisonExpr", "divExpr",
                "equalsExpr", "equalsOverAnd", "equalsOverBool", "equalsOverOr", "greaterEqExpr", "greaterExpr", "intCalc",
                "intExpr1", "lessCharExpr", "lessEqExpr", "lessExpr", "longExpr", "longExpr2", "longExpr3", "longSplitExpr",
                "longSplitExpr2", "minusExpr", "minusMinusExpr", "minusNoWhitespaceExpr", "minusPlusExpr", "modExpr",
                "multExpr", "multNoWhitespaceExpr", "negBothDiv", "negBothMod", "negDividendDiv", "negDividendMod",
                "negDivisorDiv", "negDivisorMod", "negExpr", "notequalsExpr", "notExpr", "ordAndchrExpr", "orExpr",
                "plusExpr", "plusMinusExpr", "plusNoWhitespaceExpr", "plusPlusExpr", "sequentialCount", "stringEqualsExpr"]
        )
        fun checkSourceCodeIsSyntacticallyValid(fileName: String) {
            assertTrue(isSyntaxValid("$expressionsFolderPath/$fileName.wacc"))
        }
    }

    @Nested
    inner class FunctionValidFiles {
        private val functionsFolderPath = "$validFolderPath/function"

        @Nested
        inner class NestedFunctionValidFiles {
            private val nestedFunctionsFolderPath = "$functionsFolderPath/nested_functions"

            @ParameterizedTest(name = "check {0} source code is syntactically valid")
            @ValueSource(
                strings = ["fibonacciFullRec", "fibonacciRecursive", "fixedPointRealArithmetic",
                    "functionConditionalReturn", "mutualRecursion", "printInputTriangle", "printTriangle",
                    "simpleRecursion"]
            )
            fun checkSourceCodeIsSyntacticallyValid(fileName: String) {
                assertTrue(isSyntaxValid("$nestedFunctionsFolderPath/$fileName.wacc"))
            }
        }

        @Nested
        inner class SimpleFunctionValidFiles {
            private val simpleFunctionsFolderPath = "$functionsFolderPath/simple_functions"

            @ParameterizedTest(name = "check {0} source code is syntactically valid")
            @ValueSource(
                strings = ["asciiTable", "functionDeclaration", "functionDoubleReturn", "functionIfReturns",
                    "functionManyArguments", "functionMultiReturns", "functionReturnPair", "functionSimple",
                    "functionSimpleLoop", "functionUpdateParameter", "incFunction", "negFunction", "sameArgName",
                    "sameArgName2", "sameNameAsVar"]
            )
            fun checkSourceCodeIsSyntacticallyValid(fileName: String) {
                assertTrue(isSyntaxValid("$simpleFunctionsFolderPath/$fileName.wacc"))
            }
        }
    }

    @Nested
    inner class IfValidFiles {
        private val ifValidFileFolderPath = "$validFolderPath/if"

        @ParameterizedTest(name = "check {0} source code is syntactically valid")
        @ValueSource(strings = ["if1", "if2", "if3", "if4", "if5", "if6", "ifBasic", "ifFalse", "ifTrue", "whitespace"])
        fun checkSourceCodeIsSyntacticallyValid(fileName: String) {
            assertTrue(isSyntaxValid("$ifValidFileFolderPath/$fileName.wacc"))
        }
    }

    @Nested
    inner class IOValidFiles {
        private val ioValidFileFolderPath = "$validFolderPath/IO"

        @ParameterizedTest(name = "check {0} source code is syntactically valid")
        @ValueSource(strings = ["IOLoop", "IOSequence"])
        fun checkSourceCodeIsSyntacticallyValid(fileName: String) {
            assertTrue(isSyntaxValid("$ioValidFileFolderPath/$fileName.wacc"))
        }

        @Nested
        inner class PrintValidFiles {
            private val printValidFileFolderPath = "$ioValidFileFolderPath/print"

            @ParameterizedTest(name = "check {0} source code is syntactically valid")
            @ValueSource(
                strings = ["hashInProgram", "multipleStringsAssignment", "print", "print-backspace",
                    "print-carridge-return", "printBool", "printChar", "printCharArray", "printCharAsString", "printEscChar",
                    "printInt", "println"]
            )
            fun checkSourceCodeIsSyntacticallyValid(fileName: String) {
                assertTrue(isSyntaxValid("$printValidFileFolderPath/$fileName.wacc"))
            }
        }

        @Nested
        inner class ReadValidFiles {
            private val readValidFileFolderPath = "$ioValidFileFolderPath/read"

            @ParameterizedTest(name = "check {0} source code is syntactically valid")
            @ValueSource(
                strings = ["echoBigInt", "echoBigNegInt", "echoChar", "echoInt", "echoNegInt", "echoPuncChar",
                    "read"]
            )
            fun checkSourceCodeIsSyntacticallyValid(fileName: String) {
                assertTrue(isSyntaxValid("$readValidFileFolderPath/$fileName.wacc"))
            }
        }
    }

    @Nested
    inner class PairValidFiles {
        private val pairsValidFileFolderPath = "$validFolderPath/pairs"

        @ParameterizedTest(name = "check {0} source code is syntactically valid")
        @ValueSource(
            strings = ["checkRefPair", "createPair", "createPair02", "createPair03", "createRefPair", "free",
                "linkedList", "nestedPair", "null", "printNull", "printNullPair", "printPair", "printPairOfNulls", "readPair",
                "writeFst", "writeSnd"]
        )
        fun checkSourceCodeIsSyntacticallyValid(fileName: String) {
            assertTrue(isSyntaxValid("$pairsValidFileFolderPath/$fileName.wacc"))
        }
    }

    @Nested
    inner class RuntimeErrorValidFiles {
        private val runtimeErrorValidFileFolderPath = "$validFolderPath/runtimeErr"

        @Nested
        inner class ArrayOutOfBoundsValidFiles {
            private val arrayOutOfBoundsValidFileFolderPath = "$runtimeErrorValidFileFolderPath/arrayOutOfBounds"

            @ParameterizedTest(name = "check {0} source code is syntactically valid")
            @ValueSource(strings = ["arrayNegBounds", "arrayOutOfBounds", "arrayOutOfBoundsWrite"])
            fun checkSourceCodeIsSyntacticallyValid(fileName: String) {
                assertTrue(isSyntaxValid("$arrayOutOfBoundsValidFileFolderPath/$fileName.wacc"))
            }
        }

        @Nested
        inner class DivideByZeroValidFiles {
            private val divideByZeroValidFileFolderPath = "$runtimeErrorValidFileFolderPath/divideByZero"

            @ParameterizedTest(name = "check {0} source code is syntactically valid")
            @ValueSource(strings = ["divideByZero", "divZero", "modByZero"])
            fun checkSourceCodeIsSyntacticallyValid(fileName: String) {
                assertTrue(isSyntaxValid("$divideByZeroValidFileFolderPath/$fileName.wacc"))
            }
        }

        @Nested
        inner class IntegerOverflowValidFiles {
            private val integerOverflowValidFileFolderPath = "$runtimeErrorValidFileFolderPath/integerOverflow"

            @ParameterizedTest(name = "check {0} source code is syntactically valid")
            @ValueSource(
                strings = ["intJustOverflow", "intmultOverflow", "intnegateOverflow", "intnegateOverflow2",
                    "intnegateOverflow3", "intnegateOverflow4", "intUnderflow", "intWayOverflow"]
            )
            fun checkSourceCodeIsSyntacticallyValid(fileName: String) {
                assertTrue(isSyntaxValid("$integerOverflowValidFileFolderPath/$fileName.wacc"))
            }
        }

        @Nested
        inner class NullDereferenceValidFiles {
            private val nullDereferenceValidFileFolderPath = "$runtimeErrorValidFileFolderPath/nullDereference"

            @ParameterizedTest(name = "check {0} source code is syntactically valid")
            @ValueSource(strings = ["freeNull", "readNull1", "readNull2", "setNull1", "setNull2", "useNull1", "useNull2"])
            fun checkSourceCodeIsSyntacticallyValid(fileName: String) {
                assertTrue(isSyntaxValid("$nullDereferenceValidFileFolderPath/$fileName.wacc"))
            }
        }
    }

    @Nested
    inner class ScopeValidFiles {
        private val scopeValidFileFolderPath = "$validFolderPath/scope"

        @ParameterizedTest(name = "check {0} source code is syntactically valid")
        @ValueSource(
            strings = ["ifNested1", "ifNested2", "indentationNotImportant", "intsAndKeywords", "printAllTypes",
                "scope", "scopeBasic", "scopeIfRedefine", "scopeRedefine", "scopeSimpleRedefine", "scopeVars",
                "scopeWhileNested", "scopeWhileRedefine"]
        )
        fun checkSourceCodeIsSyntacticallyValid(fileName: String) {
            assertTrue(isSyntaxValid("$scopeValidFileFolderPath/$fileName.wacc"))
        }
    }

    @Nested
    inner class SequenceValidFiles {
        private val sequenceValidFileFolderPath = "$validFolderPath/sequence"

        @ParameterizedTest(name = "check {0} source code is syntactically valid")
        @ValueSource(
            strings = ["basicSeq", "basicSeq2", "boolAssignment", "charAssignment", "exitSimple",
                "intAssignment", "intLeadingZeros", "stringAssignment"]
        )
        fun checkSourceCodeIsSyntacticallyValid(fileName: String) {
            assertTrue(isSyntaxValid("$sequenceValidFileFolderPath/$fileName.wacc"))
        }
    }

    @Nested
    inner class VariableValidFiles {
        private val variablesValidFileFolderPath = "$validFolderPath/variables"

        @ParameterizedTest(name = "check {0} source code is syntactically valid")
        @ValueSource(
            strings = ["_VarNames", "booLDeclaration", "boolDeclaration2", "capCharDeclaration",
                "charDeclaration", "charDeclaration2", "emptyStringDeclaration", "intDeclaration", "longVarNames",
                "manyVariables", "negIntDeclaration", "puncCharDeclaration", "stringDeclaration", "zeroIntDeclaration"]
        )
        fun checkSourceCodeIsSyntacticallyValid(fileName: String) {
            assertTrue(isSyntaxValid("$variablesValidFileFolderPath/$fileName.wacc"))
        }
    }

    @Nested
    inner class WhileValidFiles {
        private val whileValidFileFolderPath = "$validFolderPath/while"

        @ParameterizedTest(name = "check {0} source code is syntactically valid")
        @ValueSource(
            strings = ["fibonacciFullIt", "fibonacciIterative", "loopCharCondition", "loopIntCondition", "max",
                "min", "rmStyleAdd", "rmStyleAddIO", "whileBasic", "whileBoolFlip", "whileCount", "whileFalse"]
        )
        fun checkSourceCodeIsSyntacticallyValid(fileName: String) {
            assertTrue(isSyntaxValid("$whileValidFileFolderPath/$fileName.wacc"))
        }
    }

    @Nested
    inner class SyntaxErrorInvalidFiles {
        private val syntaxErrorFileFolderPath = "$invalidFolderPath/syntaxErr"

        @Nested
        inner class ArrayInvalidFiles {
            private val arrayInvalidFileFolderPath = "$syntaxErrorFileFolderPath/array"

            @ParameterizedTest(name = "check {0} source code is not syntactically valid")
            @ValueSource(strings = ["arrayExpr"])
            fun checkSourceCodeIsSyntacticallyValid(fileName: String) {
                assertTrue(isSyntaxInvalid("$arrayInvalidFileFolderPath/$fileName.wacc", ErrorType.SYNTAX))
            }
        }

        @Nested
        inner class BasicInvalidFiles {
            private val basicInvalidFileFolderPath = "$syntaxErrorFileFolderPath/basic"

            @ParameterizedTest(name = "check {0} source code is not syntactically valid")
            @ValueSource(
                strings = ["badComment", "badComment2", "badEscape", "beginNoend", "bgnErr", "multipleBegins",
                    "noBody", "skpErr", "unescapedChar"]
            )
            fun checkSourceCodeIsSyntacticallyValid(fileName: String) {
                assertTrue(isSyntaxInvalid("$basicInvalidFileFolderPath/$fileName.wacc", ErrorType.SYNTAX))
            }
        }

        @Nested
        inner class ExpressionInvalidFiles {
            private val expressionsInvalidFileFolderPath = "$syntaxErrorFileFolderPath/expressions"

            @ParameterizedTest(name = "check {0} source code is not syntactically valid")
            @ValueSource(strings = ["missingOperand1", "missingOperand2", "printlnConcat"])
            fun checkSourceCodeIsSyntacticallyValid(fileName: String) {
                assertTrue(isSyntaxInvalid("$expressionsInvalidFileFolderPath/$fileName.wacc", ErrorType.SYNTAX))
            }
        }

        @Nested
        inner class FunctionInvalidFiles {
            private val functionInvalidFileFolderPath = "$syntaxErrorFileFolderPath/function"

            @ParameterizedTest(name = "check {0} source code is not syntactically valid")
            @ValueSource(
                strings = ["badlyNamed", "badlyPlaced", "funcExpr", "funcExpr2", "functionConditionalNoReturn",
                    "functionEndingNotReturn", "functionLateDefine", "functionMissingCall", "functionMissingParam",
                    "functionMissingPType", "functionNoReturn", "functionReturnInLoop", "functionScopeDef",
                    "mutualRecursionNoReturn", "nobodyAfterFuncs", "thisIsNotC"]
            )
            fun checkSourceCodeIsSyntacticallyValid(fileName: String) {
                assertTrue(isSyntaxInvalid("$functionInvalidFileFolderPath/$fileName.wacc", ErrorType.SYNTAX))
            }
        }

        @Nested
        inner class IfInvalidFiles {
            private val ifInvalidFileFolderPath = "$syntaxErrorFileFolderPath/if"

            @ParameterizedTest(name = "check {0} source code is not syntactically valid")
            @ValueSource(strings = ["ifiErr", "ifNoelse", "ifNofi", "ifNothen"])
            fun checkSourceCodeIsSyntacticallyValid(fileName: String) {
                assertTrue(isSyntaxInvalid("$ifInvalidFileFolderPath/$fileName.wacc", ErrorType.SYNTAX))
            }
        }

        @Nested
        inner class PairInvalidFiles {
            private val pairsInvalidFileFolderPath = "$syntaxErrorFileFolderPath/pairs"

            @ParameterizedTest(name = "check {0} source code is not syntactically valid")
            @ValueSource(strings = ["badLookup01", "badLookup02"])
            fun checkSourceCodeIsSyntacticallyValid(fileName: String) {
                assertTrue(isSyntaxInvalid("$pairsInvalidFileFolderPath/$fileName.wacc", ErrorType.SYNTAX))
            }
        }

        @Nested
        inner class PrintInvalidFiles {
            private val printInvalidFileFolderPath = "$syntaxErrorFileFolderPath/print"

            @ParameterizedTest(name = "check {0} source code is not syntactically valid")
            @ValueSource(strings = ["printlnCharArry"])
            fun checkSourceCodeIsSyntacticallyValid(fileName: String) {
                assertTrue(isSyntaxInvalid("$printInvalidFileFolderPath/$fileName.wacc", ErrorType.SYNTAX))
            }
        }

        @Nested
        inner class SequenceInvalidFiles {
            private val sequenceInvalidFileFolderPath = "$syntaxErrorFileFolderPath/sequence"

            @ParameterizedTest(name = "check {0} source code is not syntactically valid")
            @ValueSource(strings = ["doubleSeq", "emptySeq", "endSeq", "extraSeq", "missingSeq"])
            fun checkSourceCodeIsSyntacticallyValid(fileName: String) {
                assertTrue(isSyntaxInvalid("$sequenceInvalidFileFolderPath/$fileName.wacc", ErrorType.SYNTAX))
            }
        }

        @Nested
        inner class VariableInvalidFiles {
            private val variableInvalidFileFolderPath = "$syntaxErrorFileFolderPath/variables"

            @ParameterizedTest(name = "check {0} source code is not syntactically valid")
            @ValueSource(
                strings = ["badintAssignments", "badintAssignments1", "badintAssignments2", "bigIntAssignment",
                    "varNoName"]
            )
            fun checkSourceCodeIsSyntacticallyValid(fileName: String) {
                assertTrue(isSyntaxInvalid("$variableInvalidFileFolderPath/$fileName.wacc", ErrorType.SYNTAX))
            }
        }

        @Nested
        inner class WhileInvalidFiles {
            private val whileInvalidFileFolderPath = "$syntaxErrorFileFolderPath/while"

            @ParameterizedTest(name = "check {0} source code is not syntactically valid")
            @ValueSource(strings = ["donoErr", "dooErr", "whileNodo", "whileNodone", "whilErr"])
            fun checkSourceCodeIsSyntacticallyValid(fileName: String) {
                assertTrue(isSyntaxInvalid("$whileInvalidFileFolderPath/$fileName.wacc", ErrorType.SYNTAX))
            }
        }
    }

    private fun isSyntaxValid(path: String):
            Boolean {
        val process =
            ProcessBuilder(
                "/bin/bash", "-c",
                "java -jar " +
                        "target/WACC-1.0-SNAPSHOT-jar-with-dependencies.jar " +
                        "< $path 2>&1 | wc -l"
            ).start()
        var num = 0

        try {
            val exitCode = process.waitFor()
            Assertions.assertEquals(0, exitCode)
            num = Integer.parseInt(
                IOUtils.toString(
                    process.inputStream,
                    StandardCharsets.UTF_8.name()
                ).trim()
            )
        } catch (e: InterruptedException) {
            e.printStackTrace()
        }

        // Print files that cause parsing errors
        if (num != 1) {
            println(path)
        }

        return num == 1
    }

    private fun isSyntaxInvalid(path: String, errorType: ErrorType): Boolean {
        val process =
            ProcessBuilder(
                "/bin/bash", "-c",
                "java -jar " +
                        "target/WACC-1.0-SNAPSHOT-jar-with-dependencies.jar " +
                        "< $path 2>&1"
            ).start()

        var output = ""
        var exitCode = -1

        try {
            exitCode = process.waitFor()
            output = IOUtils.toString(
                process.inputStream,
                StandardCharsets.UTF_8.name()
            )
        } catch (e: InterruptedException) {
            e.printStackTrace()
        }

        val success = (output == "#${errorType.type}_error#" && exitCode == errorType.code)

        // Print files that don't print the appropriate error msg
        if (!success) {
            println(path)
        }

        return success
    }
}
