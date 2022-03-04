package ic.doc.group15.integration

import org.apache.maven.surefire.shade.org.apache.commons.io.IOUtils
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Timeout
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths

class EmulationLocalTests {

    private val validFolder = "wacc_examples/valid"

    @Timeout(30)
    @Test
    fun advancedExceptTicTacToeAndHashTableEmulationProducesRightExitCodesAndOutput() {
        checkAssemblyFolder("$validFolder/advanced")
    }

    @Timeout(30)
    @Test
    fun arrayEmulationProducesRightExitCodesAndOutput() {
        checkAssemblyFolder("$validFolder/array")
    }

    @Timeout(30)
    @Test
    fun basicEmulationProducesRightExitCodesAndOutput() {
        checkAssemblyFolder("$validFolder/basic")
    }

    @Timeout(30)
    @Test
    fun expressionsEmulationProducesRightExitCodesAndOutput() {
        checkAssemblyFolder("$validFolder/expressions")
    }

    @Timeout(30)
    @Test
    fun functionEmulationProducesRightExitCodesAndOutput() {
        checkAssemblyFolder("$validFolder/function")
    }

    @Timeout(30)
    @Test
    fun ifEmulationProducesRightExitCodesAndOutput() {
        checkAssemblyFolder("$validFolder/if")
    }

    @Timeout(30)
    @Test
    fun ioExceptIOLoopEmulationProducesRightExitCodesAndOutput() {
        checkAssemblyFolder("$validFolder/IO")
    }

    @Timeout(30)
    @Test
    fun pairsEmulationProducesRightExitCodesAndOutput() {
        checkAssemblyFolder("$validFolder/pairs")
    }

    @Timeout(30)
    @Test
    fun runtimeErrEmulationProducesRightExitCodesAndOutput() {
        checkAssemblyFolder("$validFolder/runtimeErr")
    }

    @Timeout(30)
    @Test
    fun scopeEmulationProducesRightExitCodesAndOutput() {
        checkAssemblyFolder("$validFolder/scope")
    }

    @Timeout(30)
    @Test
    fun sequenceEmulationProducesRightExitCodesAndOutput() {
        checkAssemblyFolder("$validFolder/sequence")
    }

    @Timeout(30)
    @Test
    fun variablesEmulationProducesRightExitCodesAndOutput() {
        checkAssemblyFolder("$validFolder/variables")
    }

    @Timeout(30)
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
                    it.toString()
                )
            }
            .reduce { a, b -> a && b }
            .orElse(false)
        if (!res) {
            throw Error()
        }
    }

    private fun checkAssembly(path: String): Boolean {

        println("testing path: $path")

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

        println("compile completed")

//        println("Compilation output")
//        println(compilationOutput)

        assertTrue(setOf(0, 100, 200)
            .contains(compilationExitStatus), "./compile $path failed\n")

        val filename = path.split("/").last()
        val executable = filename.substring(0, filename.length - 4)
        val asmFilename = executable + "s"

        val emulateOnline = "./wacc_examples/refEmulate $asmFilename"
        val emulateLocal = "arm-linux-gnueabi-gcc -o $executable " +
            "-mcpu=arm1176jzf-s -mtune=arm1176jzf-s $asmFilename; " +
            "qemu-arm -L /usr/arm-linux-gnueabi $executable"

        val emulate =
            ProcessBuilder(
                "/bin/bash", "-c",
                "echo '' | $emulateOnline 2>&1"
            ).start()

        var exitCode1 = -1

        try {
            exitCode1 = emulate.waitFor()
        } catch (e: InterruptedException) {
            e.printStackTrace()
        }

        println("Emulate completed")

        val actual = IOUtils.toString(
            emulate.inputStream,
            StandardCharsets.UTF_8.name()
        ).trim()

//        println("actual:")
//        println(actual)

        assertTrue(0 == exitCode1, "refEmulate failed\n")

        val modelSolution =
            ProcessBuilder(
                "/bin/bash", "-c",
                "echo '' | ./wacc_examples/refCompile -ax $path 2>&1"
            ).start()

        var exitCode2 = -1

        try {
            exitCode2 = modelSolution.waitFor()
        } catch (e: InterruptedException) {
            e.printStackTrace()
        }

        val expected = IOUtils.toString(
            modelSolution.inputStream,
            StandardCharsets.UTF_8.name()
        ).trim()

//        println("expected:")
//        println(expected)

        assertTrue(0 == exitCode2, "refCompile failed\n")

        println("ModelSolution completed")

        val actualOutput = actual
            .split("-- Emulation Output:\n")[1]
            .split("---------------------------------------------------------------")[0]
            .trim()
        val actualExitCode = Regex("(?<=The exit code is: )[0-9]+(?=\\.)")
            .find(actual)?.value!!
//        val actualAssembly = actual
//            .split("-- Uploaded file:")[1]
//            .split("---------------------------------------------------------------\n")[1]
//            .split("---------------------------------------------------------------")[0]
//            .trim()
        val expectedOutput = expected
            .split(
                "-- Executing...\n" +
                    "===========================================================\n"
            )[1]
            .split("===========================================================")[0]
            .trim()
        val expectedExitCode = Regex("(?<=The exit code is )[0-9]+(?=\\.)")
            .find(expected)?.value!!
//        val expectedAssemblyCluttered = expected
//            .split("contents are:\n" +
//                    "===========================================================\n")[1]
//            .split("===========================================================")[0]
//        val expectedAssembly = Regex("^[0-9]+\t", RegexOption.MULTILINE)
//            .replace(expectedAssemblyCluttered, "").trim()

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

//        Code used to map expected output to .txt files

//        val lst = path.split("/")
//        val enclosingFolder = lst.subList(0, lst.size - 1).joinToString("/")
//        val txtPath = path.substring(0, path.length - 4) + "txt"
//
//        Files.createDirectories(Paths.get("model_output/$enclosingFolder"))
//
//        File("model_output/$txtPath").writeText("Exit code:\n" +
//                "$expectedExitCode\n" +
//                "Output:\n" +
//                expectedOutput)

//        println("Path: $path")
//
//        println("Expected exit code: $expectedExitCode")
//        println("Actual exit code: $actualExitCode")
//
//        println("Expected output:")
//        println(expectedOutput)
//        println("Actual output:")
//        println(actualOutput)
//
//        println("Expected assembly:")
//        println(expectedAssembly)
//        println("Actual assembly:")
//        println(actualAssembly)

        return success
    }
}
