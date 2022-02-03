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
        val res = Files.walk(Paths.get("wacc_examples/valid/advanced"))
            .filter(Files::isRegularFile)
            .filter { path -> path.toString().endsWith(".wacc") }
            .map {
                checkThatValidProgramDoesNotProduceErrorMessages(
                it.toString())
            }
            .reduce { a, b -> a && b }
            .orElse(false)

        if (!res) {
            throw Error()
        }
    }

    private fun checkThatValidProgramDoesNotProduceErrorMessages(path: String):
            Boolean {
        val process =
            ProcessBuilder(
                "/bin/sh", "-c",
                "./compile $path 2>&1 | wc -l"
            ).start()
        var num = 0
        try {
            val exitCode = process.waitFor()
            val output: String = IOUtils.toString(
                process.inputStream,
                StandardCharsets.UTF_8.name()
            ).trim()
            num = Integer.parseInt(output)
            assertEquals(0, exitCode)
        } catch (e: InterruptedException) {
            e.printStackTrace()
        }
        if (num != 1) {
            println(path)
        }

        return num == 1
    }

}
