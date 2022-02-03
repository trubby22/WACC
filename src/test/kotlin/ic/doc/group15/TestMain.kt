package ic.doc.group15

import org.apache.maven.surefire.shade.org.apache.commons.io.IOUtils
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths

class TestMain {

//    private val input1: CharStream = CharStreams.fromString("1+2+3")
//
//    @Test
//    fun correctlyParses1Plus2Plus3() {
//        assertEquals("(prog (expr (expr (expr 1) " +
//                "(binaryOper +) (expr 2)) (binaryOper +) (expr 3)) <EOF>)",
//            tree(input1))
//    }

//    @Test
//    fun parsesSkipFiles() {
//        val dir: String = "wacc_examples/valid/basic/skip"
//        File(dir).walk().map { it.toString() }.filter { it != dir }.forEach {
//            assertEquals(
//                "(program begin (stat skip) end <EOF>)",
//                tree(CharStreams.fromFileName(it)))
//        }
//    }

    @Test
    fun checkThatValidProgramsDoNotProduceErrorMessages() {
        println("Starting test")
        val res = Files.walk(Paths.get("wacc_examples/valid/advanced"))
            .filter(Files::isRegularFile)
            .filter { path -> path.toString().endsWith(".wacc") }
            .map {
                checkThatValidProgramDoesNotProduceErrorMessages(
                it.toString())
            }
            .reduce { a, b -> a && b }
            .orElse(false)

        println("Test finished")
        if (!res) {
            throw Error()
        }
    }

//    @Test
//    fun learnAboutGitLabShell() {
//        val pbs = listOf(
//            ProcessBuilder("/bin/bash", "-c", "ls"),
//            ProcessBuilder("/bin/bash", "-c", "pwd"),
//            ProcessBuilder("/bin/bash", "-c", "ls target"),
//            ProcessBuilder("/bin/bash", "-c", "which java"),
//        )
//
//        pbs.stream().forEach {
//            try {
//                val process = it.start()
//                val exitCode = process.waitFor()
//                assertEquals(0, exitCode)
//                println(IOUtils.toString(
//                    process.inputStream,
//                    StandardCharsets.UTF_8.name()
//                ).trim())
//                println()
//            } catch (e: InterruptedException) {
//                e.printStackTrace()
//            }
//        }
//
//    }

    private fun checkThatValidProgramDoesNotProduceErrorMessages(path: String):
            Boolean {
        println("Testing: $path")
        val process =
            ProcessBuilder(
                "/bin/bash", "-c",
                "java -jar target/WACC-1.0-SNAPSHOT-jar-with-dependencies" +
                        ".jar" +
                        " < $path 2>&1 | wc -l"
            ).start()
        var num = 0
        try {
            val exitCode = process.waitFor()
            val output: String = IOUtils.toString(
                process.inputStream,
                StandardCharsets.UTF_8.name()
            ).trim()
            println("Associated output:")
            println(output)
            num = Integer.parseInt(output)
            assertEquals(0, exitCode)
        } catch (e: InterruptedException) {
            e.printStackTrace()
        }

        if (num != 1) {
            println("Error in: $path")
        } else {
            println("Parsed successfully: $path")
        }

        return num == 1
    }

}
