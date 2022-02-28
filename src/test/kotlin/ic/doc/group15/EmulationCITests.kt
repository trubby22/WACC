package ic.doc.group15

import org.apache.maven.surefire.shade.org.apache.commons.io.IOUtils
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.assertEquals
import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths

class EmulationCITests {

    private val validFolder = "wacc_examples/valid"

    @Test
    fun advancedExceptTicTacToeAndHashTableEmulationProducesRightExitCodesAndOutput() {
        checkAssemblyFolder("$validFolder/advanced")
    }

    @Test
    fun arrayEmulationProducesRightExitCodesAndOutput() {
        checkAssemblyFolder("$validFolder/array")
    }

    @Test
    fun basicEmulationProducesRightExitCodesAndOutput() {
        checkAssemblyFolder("$validFolder/basic")
    }

    @Test
    fun expressionsEmulationProducesRightExitCodesAndOutput() {
        checkAssemblyFolder("$validFolder/expressions")
    }

    @Test
    fun functionEmulationProducesRightExitCodesAndOutput() {
        checkAssemblyFolder("$validFolder/function")
    }

    @Test
    fun ifEmulationProducesRightExitCodesAndOutput() {
        checkAssemblyFolder("$validFolder/if")
    }

    @Test
    fun ioExceptIOLoopEmulationProducesRightExitCodesAndOutput() {
        checkAssemblyFolder("$validFolder/IO")
    }

    @Test
    fun pairsEmulationProducesRightExitCodesAndOutput() {
        checkAssemblyFolder("$validFolder/pairs")
    }

    @Test
    fun runtimeErrEmulationProducesRightExitCodesAndOutput() {
        checkAssemblyFolder("$validFolder/runtimeErr")
    }

    @Test
    fun scopeEmulationProducesRightExitCodesAndOutput() {
        checkAssemblyFolder("$validFolder/scope")
    }

    @Test
    fun sequenceEmulationProducesRightExitCodesAndOutput() {
        checkAssemblyFolder("$validFolder/sequence")
    }

    @Test
    fun variablesEmulationProducesRightExitCodesAndOutput() {
        checkAssemblyFolder("$validFolder/variables")
    }

    @Test
    fun whileEmulationProducesRightExitCodesAndOutput() {
        checkAssemblyFolder("$validFolder/while")
    }

    private fun checkAssemblyFolder(path: String) {
        val res = Files.walk(Paths.get(path))
            .filter(Files::isRegularFile)
            .filter { path -> path.toString().endsWith(".wacc") }
            .filter { path -> !path.toString().endsWith("IOLoop.wacc") }
            .filter { path -> !path.toString().endsWith("ticTacToe.wacc") }
            .filter { path -> !path.toString().endsWith("hashTable.wacc") }
            .map {
                checkAssembly(
                    it.toString())
            }
            .reduce { a, b -> a && b }
            .orElse(false)
        if (!res) {
            throw Error()
        }
    }

    private fun checkAssembly(path: String): Boolean {

//        Compile and emulate a wacc program and check against model solution
//        coming from refCompile -x (and stored under model_output/ for speed)

//        println("Testing $path")

        val compile =
            ProcessBuilder(
                "/bin/bash", "-c",
                "java -jar " +
                        "target/WACC-1.0-SNAPSHOT-jar-with-dependencies.jar " +
                        "$path < $path"
            ).start()

        try {
            val exitCode = compile.waitFor()
            assertEquals(0, exitCode)
        } catch (e: InterruptedException) {
            e.printStackTrace()
        }

//        println("Compile completed")

        val filename = path.split("/").last()
        val executable = filename.substring(0, filename.length - 4)
        val asmFilename = executable + "s"

        val emulateLocal = "arm-linux-gnueabi-gcc -o $executable " +
                "-mcpu=arm1176jzf-s -mtune=arm1176jzf-s $asmFilename; " +
                "qemu-arm -L /usr/arm-linux-gnueabi $executable; " +
                "echo $?"

        val emulate =
            ProcessBuilder(
                "/bin/bash", "-c",
                "$emulateLocal 2>&1"
            ).start()

        var exitCode1 = -1

        try {
            exitCode1 = emulate.waitFor()
        } catch (e: InterruptedException) {
            e.printStackTrace()
        }

//        println("Emulate completed")

        val actual = IOUtils.toString(
            emulate.inputStream,
            StandardCharsets.UTF_8.name()
        ).trim()

//        println("actual:")
//        println(actual)

        assertEquals(0, exitCode1)

        val txtPath = path.substring(0, path.length - 4) + "txt"
        val expectedLst = File("model_output/$txtPath").readLines()
        val actualLst = actual.split("\n")

        val actualOutput = actualLst.subList(0, actualLst.size - 1)
            .joinToString("\n").trim()
        val actualExitCode = actualLst[actualLst.size - 1].trim()

        val expectedOutput = expectedLst.subList(3, expectedLst.size)
            .joinToString("\n").trim()
        val expectedExitCode = expectedLst[1].trim()

        val success = expectedExitCode == actualExitCode && expectedOutput == actualOutput

        if (!success) {
            print(path)
        }

        if (expectedExitCode != actualExitCode) {
            print(" (exit code mismatch)")
        }

        if (expectedOutput != actualOutput) {
            print(" (output mismatch)")
        }

        if (!success) {
            println()
        }

//        if (!success) {
//            println("Expected exit code: $expectedExitCode")
//            println("Actual exit code: $actualExitCode")
//
//            println("Expected output:")
//            println(expectedOutput)
//            println("Actual output:")
//            println(actualOutput)
//        }

        return success
    }

}
