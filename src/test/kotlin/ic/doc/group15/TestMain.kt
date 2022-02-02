package ic.doc.group15

import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CharStream

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.assertEquals

class TestMain {

    private val input1: CharStream = CharStreams.fromString("1+2+3")
    private val input2: CharStream = CharStreams.fromString("1+2 3")

    @Test
    fun correctlyParses1Plus2Plus3() {
        assertEquals("(prog (expr (expr (expr 1) " +
                "(binaryOper +) (expr 2)) (binaryOper +) (expr 3)) <EOF>)",
            tree(input1))
    }

    @Test
    fun doesNotShowErrorForErroneousInput() {
        assertEquals("(prog (expr (expr 1) (binaryOper +) (expr 2)) (expr 3) " +
                "<EOF>)", tree(input2))
    }

}
