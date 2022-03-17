package ic.doc.group15.utils

import org.apache.maven.surefire.shade.org.apache.commons.io.IOUtils
import org.junit.jupiter.api.Assertions
import java.io.File
import java.nio.charset.StandardCharsets

const val TIMEOUT: Long = 10

enum class TestMode {
    Online,
    Linux
}

class EmulationUtils {
    companion object {
        const val validFolderPath = "wacc_examples/valid"
        const val validModelOutputFolderPath = "model_output/$validFolderPath"
        private val bash = "/bin/bash"
        private val options = "-c"

        /**
         * Compile a WACC program into assembly code compatible with ARM1176JZF-S processor,
         * assemble and link it using the gcc cross-compiler with the arm-linux-gnueabi-gcc
         * package, and execute the machine code using the QEMU-ARM emulator targetting
         * the ARM1176JZF-S processor.
         */
        fun exitCodeAndOutputMatchesCI(
            fileName: String,
            filePath: String,
            resultPath: String
        ) : Boolean {
            return exitCodeAndOutputMatches(fileName, filePath, resultPath, TestMode.Linux)
        }

        fun exitCodeAndOutputMatchesLocal(
            fileName: String,
            filePath: String,
            resultPath: String
        ) : Boolean {
            return exitCodeAndOutputMatches(fileName, filePath, resultPath,
                TestMode.Online)
        }

        private fun exitCodeAndOutputMatches(
            fileName: String,
            filePath: String,
            resultPath: String,
            testMode: TestMode
        ): Boolean {
            compile(filePath)
            val actualExitCode: Int
            val actualOutput: String
            when (testMode) {
                TestMode.Online -> {
                    val (fst, snd) = emulateOnline(fileName)
                    actualExitCode = fst
                    actualOutput = snd
                }
                TestMode.Linux -> {
                    val (fst, snd) = emulateLinux(fileName)
                    actualExitCode = fst
                    actualOutput = snd
                }
            }
            val (expectedExitCode, expectedOutput) = getExpectedResult(resultPath)

            val exitCodeMatches = (expectedExitCode == actualExitCode)
            val outputMatches = (expectedOutput == actualOutput)

            return exitCodeMatches && outputMatches
        }

        private fun getExpectedResult(path: String): Pair<Int, String> {
            val expectedList = File(path).readLines()
            val expectedOutput = expectedList.subList(3, expectedList.size)
                .joinToString("\n")
            val expectedExitCode = expectedList[1].trim().toInt()

            return Pair(expectedExitCode, expectedOutput)
        }

        private fun compile(path: String) {
            val compilation = ProcessBuilder(
                bash,
                options,
                "./compile $path"
            ).start()
            val compilationExitStatus = compilation.waitFor()
            val compilationOutput = IOUtils.toString(
                compilation.inputStream,
                StandardCharsets.UTF_8.name()
            )
            Assertions.assertTrue(
                setOf(0, 100, 200)
                    .contains(compilationExitStatus), "./compile failed with " +
                        "exit status $compilationExitStatus\n"
            )
        }

        private fun emulateLinux(
            fileName: String
        ): Pair<Int, String> {
            
            val emulationOutput = emulateLinuxHelper(fileName)

            val actualList = emulationOutput.split("\n")
            val actualOutput =
                actualList.subList(0, actualList.size - 1).joinToString("\n")
            val actualExitCode = actualList[actualList.size - 1].trim().toInt()

            return Pair(actualExitCode, actualOutput)
        }

        private fun emulateOnline(
            fileName: String
        ) : Pair<Int, String> {

            val emulationOutput = emulateOnlineHelper(fileName)

            val output = emulationOutput
                .split("-- Emulation Output:\n")[1]
                .split("---------------------------------------------------------------")[0]
                .trim()
            val exitCode = Regex("(?<=The exit code is: )[0-9]+(?=\\.)")
                .find(emulationOutput)?.value!!.toInt()

            return Pair(exitCode, output)
        }

        private fun emulateOnlineHelper(fileName: String): String {
            val emulateOnline = "./wacc_examples/refEmulate $fileName.s"
            val emulate =
                ProcessBuilder(
                    bash, options,
                    "echo '' | $emulateOnline 2>&1"
                ).start()
            val exitCode = emulate.waitFor()
            val output = IOUtils.toString(
                emulate.inputStream,
                StandardCharsets.UTF_8.name()
            ).trim()
            Assertions.assertTrue(0 == exitCode, "refEmulate failed\n")
            return output
        }

        private fun emulateLinuxHelper(fileName: String): String {
            val createExecutable = "arm-linux-gnueabi-gcc -o $fileName " +
                    "-mcpu=arm1176jzf-s -mtune=arm1176jzf-s $fileName.s"
            val execute =
                "echo '' | qemu-arm -L /usr/arm-linux-gnueabi $fileName"
            val echoExitCode = "echo \$?"
            val emulation = ProcessBuilder(
                bash, options,
                "$createExecutable; $execute; $echoExitCode 2>&1"
            ).start()
            val emulationExitStatus = emulation.waitFor()
            val emulationOutput = IOUtils.toString(
                emulation.inputStream,
                StandardCharsets.UTF_8.name()
            ).trim()
            Assertions.assertTrue(
                0 == emulationExitStatus,
                "Emulating using qemu failed\n"
            )
            return emulationOutput
        }
    }
}