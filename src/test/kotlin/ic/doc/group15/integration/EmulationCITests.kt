package ic.doc.group15.integration

import org.apache.maven.surefire.shade.org.apache.commons.io.IOUtils
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Disabled
import org.junit.jupiter.api.Nested
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ValueSource
import java.io.File
import java.nio.charset.StandardCharsets

class EmulationCITests {
    private val validFolderPath = "wacc_examples/valid"
    private val validModelOutputFolderPath = "model_output/$validFolderPath"

    //@Disabled
    @Nested
    inner class ArrayValidFiles {
        private val arrayFolderPath = "$validFolderPath/array"
        private val arrayResultFolderPath = "$validModelOutputFolderPath/array"

        @ParameterizedTest(name = "execution of assembly code generated from {0} source code produces expected exit code and output")
        @ValueSource(
            strings = [
                "array", "arrayBasic", "arrayEmpty", "arrayLength", "arrayLookup", "arrayNested",
                "arrayPrint", "arraySimple", "modifyString", "printRef"
            ]
        )
        fun testExecutionProducesExpectedExitCodeAndOutput(fileName: String) {
            val filePath = "$arrayFolderPath/$fileName.wacc"
            val resultPath = "$arrayResultFolderPath/$fileName.txt"

            assertTrue(exitCodeAndOutputMatches(fileName, filePath, resultPath))
        }
    }

    @Nested
    inner class BasicValidFiles {
        private val basicFolderPath = "$validFolderPath/basic"
        private val basicResultFolderPath = "$validModelOutputFolderPath/basic"

        
        @Nested
        inner class ExitValidFiles {
            private val exitFolderPath = "$basicFolderPath/exit"
            private val exitResultFolderPath = "$basicResultFolderPath/exit"

            @ParameterizedTest(name = "execution of assembly code generated from {0} source code produces expected exit code and output")
            @ValueSource(strings = ["exit-1", "exitBasic", "exitBasic2", "exitWrap"])
            fun testExecutionProducesExpectedExitCodeAndOutput(fileName: String) {
                val filePath = "$exitFolderPath/$fileName.wacc"
                val resultPath = "$exitResultFolderPath/$fileName.txt"

                assertTrue(exitCodeAndOutputMatches(fileName, filePath, resultPath))
            }
        }

        @Nested
        inner class SkipValidFiles {
            private val skipFolderPath = "$basicFolderPath/skip"
            private val skipResultFolderPath = "$basicResultFolderPath/skip"

            @ParameterizedTest(name = "execution of assembly code generated from {0} source code produces expected exit code and output")
            @ValueSource(strings = ["comment", "commentInLine", "skip"])
            fun testExecutionProducesExpectedExitCodeAndOutput(fileName: String) {
                val filePath = "$skipFolderPath/$fileName.wacc"
                val resultPath = "$skipResultFolderPath/$fileName.txt"

                assertTrue(exitCodeAndOutputMatches(fileName, filePath, resultPath))
            }
        }
    }

    //@Disabled
    @Nested
    inner class ExpressionValidFiles {
        private val expressionsFolderPath = "$validFolderPath/expressions"
        private val expressionsResultFolderPath =
            "$validModelOutputFolderPath/expressions"

        @ParameterizedTest(name = "execution of assembly code generated from {0} source code produces expected exit code and output")
        @ValueSource(
            strings = [
                "andExpr", "andOverOrExpr", "boolCalc", "boolExpr1", "charComparisonExpr", "divExpr",
                "equalsExpr", "equalsOverAnd", "equalsOverBool", "equalsOverOr", "greaterEqExpr", "greaterExpr", "intCalc",
                "intExpr1", "lessCharExpr", "lessEqExpr", "lessExpr", "longExpr", "longExpr2", "longExpr3", "longSplitExpr",
                "longSplitExpr2", "minusExpr", "minusMinusExpr", "minusNoWhitespaceExpr", "minusPlusExpr", "modExpr",
                "multExpr", "multNoWhitespaceExpr", "negBothDiv", "negBothMod", "negDividendDiv", "negDividendMod",
                "negDivisorDiv", "negDivisorMod", "negExpr", "notequalsExpr", "notExpr", "ordAndchrExpr", "orExpr",
                "plusExpr", "plusMinusExpr", "plusNoWhitespaceExpr", "plusPlusExpr", "sequentialCount", "stringEqualsExpr"
            ]
        )
        fun testExecutionProducesExpectedExitCodeAndOutput(fileName: String) {
            val filePath = "$expressionsFolderPath/$fileName.wacc"
            val resultPath = "$expressionsResultFolderPath/$fileName.txt"

            assertTrue(exitCodeAndOutputMatches(fileName, filePath, resultPath))
        }
    }

    //@Disabled
    @Nested
    inner class FunctionValidFiles {
        private val functionsFolderPath = "$validFolderPath/function"
        private val functionsResultFolderPath =
            "$validModelOutputFolderPath/function"

        @Nested
        inner class NestedFunctionValidFiles {
            private val nestedFunctionsFolderPath =
                "$functionsFolderPath/nested_functions"
            private val nestedFunctionsResultFolderPath =
                "$functionsResultFolderPath/nested_functions"

            @ParameterizedTest(name = "execution of assembly code generated from {0} source code produces expected exit code and output")
            @ValueSource(
                strings = [
                    "fibonacciFullRec", "fibonacciRecursive", "fixedPointRealArithmetic",
                    "functionConditionalReturn", "mutualRecursion", "printInputTriangle", "printTriangle",
                    "simpleRecursion"
                ]
            )
            fun testExecutionProducesExpectedExitCodeAndOutput(fileName: String) {
                val filePath = "$nestedFunctionsFolderPath/$fileName.wacc"
                val resultPath = "$nestedFunctionsResultFolderPath/$fileName.txt"

                assertTrue(exitCodeAndOutputMatches(fileName, filePath, resultPath))
            }
        }

        @Nested
        inner class SimpleFunctionValidFiles {
            private val simpleFunctionsFolderPath =
                "$functionsFolderPath/simple_functions"
            private val simpleFunctionsResultFolderPath =
                "$functionsResultFolderPath/simple_functions"

            @ParameterizedTest(name = "execution of assembly code generated from {0} source code produces expected exit code and output")
            @ValueSource(
                strings = [
                    "asciiTable", "functionDeclaration", "functionDoubleReturn", "functionIfReturns",
                    "functionManyArguments", "functionMultiReturns", "functionReturnPair", "functionSimple",
                    "functionSimpleLoop", "functionUpdateParameter", "incFunction", "negFunction", "sameArgName",
                    "sameArgName2", "sameNameAsVar"
                ]
            )
            fun testExecutionProducesExpectedExitCodeAndOutput(fileName: String) {
                val filePath = "$simpleFunctionsFolderPath/$fileName.wacc"
                val resultPath = "$simpleFunctionsResultFolderPath/$fileName.txt"

                assertTrue(exitCodeAndOutputMatches(fileName, filePath, resultPath))
            }
        }
    }

    //@Disabled
    @Nested
    inner class IfValidFiles {
        private val ifFolderPath = "$validFolderPath/if"
        private val ifResultFolderPath = "$validModelOutputFolderPath/if"

        @ParameterizedTest(name = "execution of assembly code generated from {0} source code produces expected exit code and output")
        @ValueSource(strings = ["if1", "if2", "if3", "if4", "if5", "if6", "ifBasic", "ifFalse", "ifTrue", "whitespace"])
        fun testExecutionProducesExpectedExitCodeAndOutput(fileName: String) {
            val filePath = "$ifFolderPath/$fileName.wacc"
            val resultPath = "$ifResultFolderPath/$fileName.txt"

            assertTrue(exitCodeAndOutputMatches(fileName, filePath, resultPath))
        }
    }

    //@Disabled
    @Nested
    inner class IOValidFiles {
        private val ioFolderPath = "$validFolderPath/IO"
        private val ioResultFolderPath = "$validModelOutputFolderPath/IO"

        // Note: IOLoop is left out until a separate function is written to
        // handle interactive input using "echo input | refCompile/command"
        @ParameterizedTest(name = "execution of assembly code generated from {0} source code produces expected exit code and output")
        @ValueSource(strings = ["IOSequence"])
        fun testExecutionProducesExpectedExitCodeAndOutput(fileName: String) {
            val filePath = "$ioFolderPath/$fileName.wacc"
            val resultPath = "$ioResultFolderPath/$fileName.txt"

            assertTrue(exitCodeAndOutputMatches(fileName, filePath, resultPath))
        }

        @Nested
        inner class PrintValidFiles {
            private val printFolderPath = "$ioFolderPath/print"
            private val printResultFolderPath = "$ioResultFolderPath/print"

            @ParameterizedTest(name = "execution of assembly code generated from {0} source code produces expected exit code and output")
            @ValueSource(
                strings = [
                    "hashInProgram", "multipleStringsAssignment", "print", "print-backspace",
                    "print-carridge-return", "printBool", "printChar", "printCharArray", "printCharAsString", "printEscChar",
                    "printInt", "println"
                ]
            )
            fun testExecutionProducesExpectedExitCodeAndOutput(fileName: String) {
                val filePath = "$printFolderPath/$fileName.wacc"
                val resultPath = "$printResultFolderPath/$fileName.txt"

                assertTrue(exitCodeAndOutputMatches(fileName, filePath, resultPath))
            }
        }

        @Nested
        inner class ReadValidFiles {
            private val readFolderPath = "$ioFolderPath/read"
            private val readResultFolderPath = "$ioResultFolderPath/read"

            @ParameterizedTest(name = "execution of assembly code generated from {0} source code produces expected exit code and output")
            @ValueSource(
                strings = [
                    "echoBigInt", "echoBigNegInt", "echoChar", "echoInt", "echoNegInt", "echoPuncChar",
                    "read"
                ]
            )
            fun testExecutionProducesExpectedExitCodeAndOutput(fileName: String) {
                val filePath = "$readFolderPath/$fileName.wacc"
                val resultPath = "$readResultFolderPath/$fileName.txt"

                assertTrue(exitCodeAndOutputMatches(fileName, filePath, resultPath))
            }
        }
    }

    //@Disabled
    @Nested
    inner class PairValidFiles {
        private val pairsFolderPath = "$validFolderPath/pairs"
        private val pairsResultFolderPah = "$validModelOutputFolderPath/pairs"

        @ParameterizedTest(name = "execution of assembly code generated from {0} source code produces expected exit code and output")
        @ValueSource(
            strings = [
                "checkRefPair", "createPair", "createPair02", "createPair03", "createRefPair", "free",
                "linkedList", "nestedPair", "null", "printNull", "printNullPair", "printPair", "printPairOfNulls", "readPair",
                "writeFst", "writeSnd"
            ]
        )
        fun testExecutionProducesExpectedExitCodeAndOutput(fileName: String) {
            val filePath = "$pairsFolderPath/$fileName.wacc"
            val resultPath = "$pairsResultFolderPah/$fileName.txt"

            assertTrue(exitCodeAndOutputMatches(fileName, filePath, resultPath))
        }
    }

    //@Disabled
    @Nested
    inner class RuntimeErrorValidFiles {
        private val runtimeErrorFolderPath = "$validFolderPath/runtimeErr"
        private val runtimeErrorResultFolderPath =
            "$validModelOutputFolderPath/runtimeErr"

        @Nested
        inner class ArrayOutOfBoundsValidFiles {
            private val arrayOutOfBoundsFolderPath =
                "$runtimeErrorFolderPath/arrayOutOfBounds"
            private val arrayOutOfBoundsResultFolderPath =
                "$runtimeErrorResultFolderPath/arrayOutOfBounds"

            @ParameterizedTest(name = "execution of assembly code generated from {0} source code produces expected exit code and output")
            @ValueSource(strings = ["arrayNegBounds", "arrayOutOfBounds", "arrayOutOfBoundsWrite"])
            fun testExecutionProducesExpectedExitCodeAndOutput(fileName: String) {
                val filePath = "$arrayOutOfBoundsFolderPath/$fileName.wacc"
                val resultPath = "$arrayOutOfBoundsResultFolderPath/$fileName.txt"

                assertTrue(exitCodeAndOutputMatches(fileName, filePath, resultPath))
            }
        }

        @Nested
        inner class DivideByZeroValidFiles {
            private val divideByZeroFolderPath =
                "$runtimeErrorFolderPath/divideByZero"
            private val divideByZeroResultFolderPath =
                "$runtimeErrorResultFolderPath/divideByZero"

            @ParameterizedTest(name = "execution of assembly code generated from {0} source code produces expected exit code and output")
            @ValueSource(strings = ["divideByZero", "divZero", "modByZero"])
            fun testExecutionProducesExpectedExitCodeAndOutput(fileName: String) {
                val filePath = "$divideByZeroFolderPath/$fileName.wacc"
                val resultPath = "$divideByZeroResultFolderPath/$fileName.txt"

                assertTrue(exitCodeAndOutputMatches(fileName, filePath, resultPath))
            }
        }

        @Nested
        inner class IntegerOverflowValidFiles {
            private val integerOverflowFolderPath =
                "$runtimeErrorFolderPath/integerOverflow"
            private val integerOverflowResultFolderPath =
                "$runtimeErrorResultFolderPath/integerOverflow"

            @ParameterizedTest(name = "execution of assembly code generated from {0} source code produces expected exit code and output")
            @ValueSource(
                strings = [
                    "intJustOverflow", "intmultOverflow", "intnegateOverflow", "intnegateOverflow2",
                    "intnegateOverflow3", "intnegateOverflow4", "intUnderflow", "intWayOverflow"
                ]
            )
            fun testExecutionProducesExpectedExitCodeAndOutput(fileName: String) {
                val filePath = "$integerOverflowFolderPath/$fileName.wacc"
                val resultPath = "$integerOverflowResultFolderPath/$fileName.txt"

                assertTrue(exitCodeAndOutputMatches(fileName, filePath, resultPath))
            }
        }

        @Nested
        inner class NullDereferenceValidFiles {
            private val nullDereferenceFolderPath =
                "$runtimeErrorFolderPath/nullDereference"
            private val nullDereferenceResultFolderPath =
                "$runtimeErrorResultFolderPath/nullDereference"

            @ParameterizedTest(name = "execution of assembly code generated from {0} source code produces expected exit code and output")
            @ValueSource(strings = ["freeNull", "readNull1", "readNull2", "setNull1", "setNull2", "useNull1", "useNull2"])
            fun testExecutionProducesExpectedExitCodeAndOutput(fileName: String) {
                val filePath = "$nullDereferenceFolderPath/$fileName.wacc"
                val resultPath = "$nullDereferenceResultFolderPath/$fileName.txt"

                assertTrue(exitCodeAndOutputMatches(fileName, filePath, resultPath))
            }
        }
    }

    //@Disabled
    @Nested
    inner class ScopeValidFiles {
        private val scopeFolderPath = "$validFolderPath/scope"
        private val scopeResultFolderPath = "$validModelOutputFolderPath/scope"

        @ParameterizedTest(name = "execution of assembly code generated from {0} source code produces expected exit code and output")
        @ValueSource(
            strings = [
                "ifNested1", "ifNested2", "indentationNotImportant", "intsAndKeywords", "printAllTypes",
                "scope", "scopeBasic", "scopeIfRedefine", "scopeRedefine", "scopeSimpleRedefine", "scopeVars",
                "scopeWhileNested", "scopeWhileRedefine"
            ]
        )
        fun testExecutionProducesExpectedExitCodeAndOutput(fileName: String) {
            val filePath = "$scopeFolderPath/$fileName.wacc"
            val resultPath = "$scopeResultFolderPath/$fileName.txt"

            assertTrue(exitCodeAndOutputMatches(fileName, filePath, resultPath))
        }
    }

    //@Disabled
    @Nested
    inner class SequenceValidFiles {
        private val sequenceFolderPath = "$validFolderPath/sequence"
        private val sequenceResultFolderPath =
            "$validModelOutputFolderPath/sequence"

        @ParameterizedTest(name = "execution of assembly code generated from {0} source code produces expected exit code and output")
        @ValueSource(
            strings = [
                "basicSeq", "basicSeq2", "boolAssignment", "charAssignment", "exitSimple",
                "intAssignment", "intLeadingZeros", "stringAssignment"
            ]
        )
        fun testExecutionProducesExpectedExitCodeAndOutput(fileName: String) {
            val filePath = "$sequenceFolderPath/$fileName.wacc"
            val resultPath = "$sequenceResultFolderPath/$fileName.txt"

            assertTrue(exitCodeAndOutputMatches(fileName, filePath, resultPath))
        }
    }

    @Nested
    inner class VariableValidFiles {
        private val variablesFolderPath = "$validFolderPath/variables"
        private val variablesResultFolderPath =
            "$validModelOutputFolderPath/variables"

        @ParameterizedTest(name = "execution of assembly code generated from {0} source code produces expected exit code and output")
        @ValueSource(
            strings = [
                "_VarNames", "boolDeclaration", "boolDeclaration2", "capCharDeclaration",
                "charDeclaration", "charDeclaration2", "emptyStringDeclaration", "intDeclaration", "longVarNames",
                "manyVariables", "negIntDeclaration", "puncCharDeclaration", "stringDeclaration", "zeroIntDeclaration"
            ]
        )
        fun testExecutionProducesExpectedExitCodeAndOutput(fileName: String) {
            val filePath = "$variablesFolderPath/$fileName.wacc"
            val resultPath = "$variablesResultFolderPath/$fileName.txt"

            assertTrue(exitCodeAndOutputMatches(fileName, filePath, resultPath))
        }
    }

    //@Disabled
    @Nested
    inner class WhileValidFiles {
        private val whileFolderPath = "$validFolderPath/while"
        private val whileResultFolderPath = "$validModelOutputFolderPath/while"

        @ParameterizedTest(name = "execution of assembly code generated from {0} source code produces expected exit code and output")
        @ValueSource(
            strings = [
                "fibonacciFullIt", "fibonacciIterative", "loopCharCondition", "loopIntCondition", "max",
                "min", "rmStyleAdd", "rmStyleAddIO", "whileBasic", "whileBoolFlip", "whileCount", "whileFalse"
            ]
        )
        fun testExecutionProducesExpectedExitCodeAndOutput(fileName: String) {
            val filePath = "$whileFolderPath/$fileName.wacc"
            val resultPath = "$whileResultFolderPath/$fileName.txt"

            assertTrue(exitCodeAndOutputMatches(fileName, filePath, resultPath))
        }
    }

    // TODO: compile internally by generating AST, generating code and saving it into a temporary location
    /**
     * Compile a WACC program into assembly code compatible with ARM1176JZF-S processor,
     * assemble and link it using the gcc cross-compiler with the arm-linux-gnueabi-gcc
     * package, and execute the machine code using the QEMU-ARM emulator targetting
     * the ARM1176JZF-S processor.
     */
    private fun compileAndExecute(
        fileName: String,
        path: String
    ): Pair<Int, List<String>> {
        val compilation = ProcessBuilder(
            "/bin/bash",
            "-c",
            "./compile $path"
        ).start()

        var compilationExitStatus = -1

        try {
            compilationExitStatus = compilation.waitFor()
        } catch (e: InterruptedException) {
            e.printStackTrace()
        }

        val compilationOutput = IOUtils.toString(
            compilation.inputStream,
            StandardCharsets.UTF_8.name()
        )

        assertTrue(0 == compilationExitStatus, "./compile failed\n")

        val createExecutable = "arm-linux-gnueabi-gcc -o $fileName " +
            "-mcpu=arm1176jzf-s -mtune=arm1176jzf-s $fileName.s"
        val execute = "echo '' | qemu-arm -L /usr/arm-linux-gnueabi $fileName"
        val echoExitCode = "echo \$?"

        val emulation = ProcessBuilder(
            "/bin/bash", "-c",
            "$createExecutable; $execute; $echoExitCode 2>&1"
        ).start()

        var emulationExitStatus = -1
        try {
            emulationExitStatus = emulation.waitFor()
        } catch (e: InterruptedException) {
            e.printStackTrace()
        }

        val emulationOutput = IOUtils.toString(
            emulation.inputStream,
            StandardCharsets.UTF_8.name()
        ).trim()

        assertTrue(0 == emulationExitStatus, "Emulating using qemu failed\n")

        val actualList = emulationOutput.split("\n")

        val actualOutput =
            actualList.subList(0, actualList.size - 1)
        val actualExitCode = actualList[actualList.size - 1].trim().toInt()

        return Pair(actualExitCode, actualOutput)
    }

    private fun getExpectedResult(path: String): Pair<Int, List<String>> {
        val expectedList = File(path).readLines()
        val expectedOutput = expectedList.subList(3, expectedList.size)
        val expectedExitCode = expectedList[1].trim().toInt()

        return Pair(expectedExitCode, expectedOutput)
    }

    private fun exitCodeAndOutputMatches(
        fileName: String,
        filePath: String,
        resultPath: String
    ): Boolean {
        val (actualExitCode, actualOutput) = compileAndExecute(fileName, filePath)
        val (expectedExitCode, expectedOutput) = getExpectedResult(resultPath)

        val exitCodeMatches = (expectedExitCode == actualExitCode)
        val outputMatches = (expectedOutput == actualOutput)

        return exitCodeMatches && outputMatches
    }
}
