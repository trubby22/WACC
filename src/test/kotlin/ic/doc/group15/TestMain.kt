package ic.doc.group15

import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CharStream

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.assertEquals
import java.io.File

class TestMain {

//    private val input1: CharStream = CharStreams.fromString("1+2+3")
//
//    @Test
//    fun correctlyParses1Plus2Plus3() {
//        assertEquals("(prog (expr (expr (expr 1) " +
//                "(binaryOper +) (expr 2)) (binaryOper +) (expr 3)) <EOF>)",
//            tree(input1))
//    }

    @Test
    fun parsesSkipFiles() {
        val dir: String = "wacc_examples/valid/basic/skip"
        File(dir).walk().map { it.toString() }.filter { it != dir }.forEach {
            assertEquals(
                "(program begin (stat skip) end <EOF>)",
                tree(CharStreams.fromFileName(it)))
        }
    }

}
