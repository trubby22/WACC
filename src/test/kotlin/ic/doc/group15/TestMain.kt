package ic.doc.group15

import org.apache.maven.surefire.shade.org.apache.commons.io.IOUtils
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.assertEquals
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths

class TestMain {

    private val baseFolder = "wacc_examples/valid"

    @Test
    fun advancedParsingValidFilesInProducesNoErrors() {
        checkFolder("$baseFolder/advanced")
    }

    @Test
    fun arrayParsingValidFilesInProducesNoErrors() {
        checkFolder("$baseFolder/array")
    }

    @Test
    fun basicParsingValidFilesInProducesNoErrors() {
        checkFolder("$baseFolder/basic")
    }

    @Test
    fun expressionsParsingValidFilesInProducesNoErrors() {
        checkFolder("$baseFolder/expressions")
    }

    @Test
    fun functionParsingValidFilesInProducesNoErrors() {
        checkFolder("$baseFolder/function")
    }

    @Test
    fun ifParsingValidFilesInProducesNoErrors() {
        checkFolder("$baseFolder/if")
    }

    @Test
    fun ioParsingValidFilesInProducesNoErrors() {
        checkFolder("$baseFolder/IO")
    }

    @Test
    fun pairsParsingValidFilesInProducesNoErrors() {
        checkFolder("$baseFolder/pairs")
    }

    @Test
    fun runtimeErrParsingValidFilesInProducesNoErrors() {
        checkFolder("$baseFolder/runtimeErr")
    }

    @Test
    fun scopeParsingValidFilesInProducesNoErrors() {
        checkFolder("$baseFolder/scope")
    }

    @Test
    fun sequenceParsingValidFilesInProducesNoErrors() {
        checkFolder("$baseFolder/sequence")
    }

    @Test
    fun variablesParsingValidFilesInProducesNoErrors() {
        checkFolder("$baseFolder/variables")
    }

    @Test
    fun whileParsingValidFilesInProducesNoErrors() {
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
