package ic.doc.group15

import org.apache.maven.surefire.shade.org.apache.commons.io.IOUtils
import org.junit.Test
import org.junit.jupiter.api.Assertions.assertEquals
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths

class TestMain {

    private val baseFolder = "wacc_examples/valid"

    @Test
    fun parsingValidFilesInAdvancedProducesNoErrors() {
        checkFolder("$baseFolder/advanced")
    }

    @Test
    fun parsingValidFilesInArrayProducesNoErrors() {
        checkFolder("$baseFolder/array")
    }

    @Test
    fun parsingValidFilesInBasicProducesNoErrors() {
        checkFolder("$baseFolder/basic")
    }

    @Test
    fun parsingValidFilesInExpressionsProducesNoErrors() {
        checkFolder("$baseFolder/expressions")
    }

    @Test
    fun parsingValidFilesInFunctionProducesNoErrors() {
        checkFolder("$baseFolder/function")
    }

    @Test
    fun parsingValidFilesInIfProducesNoErrors() {
        checkFolder("$baseFolder/if")
    }

    @Test
    fun parsingValidFilesInIoProducesNoErrors() {
        checkFolder("$baseFolder/IO")
    }

    @Test
    fun parsingValidFilesInPairsProducesNoErrors() {
        checkFolder("$baseFolder/pairs")
    }

    @Test
    fun parsingValidFilesInProducesNoErrors() {
        checkFolder("$baseFolder/")
    }

    @Test
    fun parsingValidFilesInRuntimeErrProducesNoErrors() {
        checkFolder("$baseFolder/runtimeErr")
    }

    @Test
    fun parsingValidFilesInScopeProducesNoErrors() {
        checkFolder("$baseFolder/scope")
    }

    @Test
    fun parsingValidFilesInSequenceProducesNoErrors() {
        checkFolder("$baseFolder/sequence")
    }

    @Test
    fun parsingValidFilesInVariablesProducesNoErrors() {
        checkFolder("$baseFolder/variables")
    }

    @Test
    fun parsingValidFilesInWhileProducesNoErrors() {
        checkFolder("$baseFolder/while")
    }

    private fun checkFolder(path: String) {
        val res = Files.walk(Paths.get(path))
            .filter(Files::isRegularFile)
            .filter { path -> path.toString().endsWith(".wacc") }
            .map {
                checkFile(
                it.toString())
            }
            .reduce { a, b -> a && b }
            .orElse(false)
        if (!res) {
            throw Error()
        }
    }

    private fun checkFile(path: String):
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
