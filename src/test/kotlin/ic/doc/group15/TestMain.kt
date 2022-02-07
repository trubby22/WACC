package ic.doc.group15

import org.apache.maven.surefire.shade.org.apache.commons.io.IOUtils
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.assertEquals
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths

class TestMain {

    enum class ErrorType(val type: String, val code: Int) {
        SYNTAX("syntax", 100),
        SEMANTICS("semantics", 200)
    }

    private val validFolder = "wacc_examples/valid"

    @Test
    fun advancedValidFilesParsingProducesNoErrors() {
        checkValidFolder("$validFolder/advanced")
    }

    @Test
    fun arrayValidFilesParsingProducesNoErrors() {
        checkValidFolder("$validFolder/array")
    }

    @Test
    fun basicValidFilesParsingProducesNoErrors() {
        checkValidFolder("$validFolder/basic")
    }

    @Test
    fun expressionsValidFilesParsingProducesNoErrors() {
        checkValidFolder("$validFolder/expressions")
    }

    @Test
    fun functionValidFilesParsingProducesNoErrors() {
        checkValidFolder("$validFolder/function")
    }

    @Test
    fun ifValidFilesParsingProducesNoErrors() {
        checkValidFolder("$validFolder/if")
    }

    @Test
    fun ioValidFilesParsingProducesNoErrors() {
        checkValidFolder("$validFolder/IO")
    }

    @Test
    fun pairsValidFilesParsingProducesNoErrors() {
        checkValidFolder("$validFolder/pairs")
    }

    @Test
    fun runtimeErrValidFilesParsingProducesNoErrors() {
        checkValidFolder("$validFolder/runtimeErr")
    }

    @Test
    fun scopeValidFilesParsingProducesNoErrors() {
        checkValidFolder("$validFolder/scope")
    }

    @Test
    fun sequenceValidFilesParsingProducesNoErrors() {
        checkValidFolder("$validFolder/sequence")
    }

    @Test
    fun variablesValidFilesParsingProducesNoErrors() {
        checkValidFolder("$validFolder/variables")
    }

    @Test
    fun whileValidFilesParsingProducesNoErrors() {
        checkValidFolder("$validFolder/while")
    }

    @Test
    fun syntacticallyInvalidFilesExceptBigIntProduceExpectedErrorMessage() {
        checkInvalidFolder("wacc_examples/invalid/syntaxErr", ErrorType.SYNTAX)
    }

//    @Test
//    fun semanticallyInvalidFilesProduceExpectedErrorMessage() {
//        checkInvalidFolder("wacc_examples/invalid/semanticErr", ErrorType
//            .SEMANTICS)
//    }

    private fun checkInvalidFolder(path: String, errorType: ErrorType) {
        val res = Files.walk(Paths.get(path))
            .filter(Files::isRegularFile)
            .filter { path -> path.toString().endsWith(".wacc") }
            .filter { path -> !path.endsWith("bigIntAssignment.wacc") }
            .map {
                checkInvalidFile(
                    it.toString(), errorType)
            }
            .reduce { a, b -> a && b }
            .orElse(false)
        if (!res) {
            throw Error()
        }
    }

    private fun checkInvalidFile(path: String, errorType: ErrorType): Boolean {
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

        val success = (
                output ==
                "#${errorType.type}_error#"
                && exitCode == errorType.code)

//        Print files that don't print the appropriate error msg
        if (!success) {
            println(path)
        }

        return success
    }

    private fun checkValidFolder(path: String) {
        val res = Files.walk(Paths.get(path))
            .filter(Files::isRegularFile)
            .filter { path -> path.toString().endsWith(".wacc") }
            .map {
                checkValidFile(
                it.toString())
            }
            .reduce { a, b -> a && b }
            .orElse(false)
        if (!res) {
            throw Error()
        }
    }

    private fun checkValidFile(path: String):
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
            assertEquals(0, exitCode)
            num = Integer.parseInt(IOUtils.toString(
                process.inputStream,
                StandardCharsets.UTF_8.name()
            ).trim())
        } catch (e: InterruptedException) {
            e.printStackTrace()
        }

//        Print files that cause parsing errors
        if (num != 1) {
            println(path)
        }

        return num == 1
    }

}
