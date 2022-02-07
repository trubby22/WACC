package ic.doc.group15

import ic.doc.group15.antlr.BasicLexer
import ic.doc.group15.antlr.BasicParser
import org.antlr.v4.runtime.CharStream
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream

fun tree(input: CharStream): String {
    val lexer = BasicLexer(input)
    val tokens = CommonTokenStream(lexer)
    val parser = BasicParser(tokens)
    parser.removeErrorListeners()
    parser.addErrorListener(MyErrorListener())
//    parser.errorHandler = MyErrorStrategy()
    val tree = parser.program()

    return tree.toStringTree(parser)
}

fun main() {
    println(tree(CharStreams.fromStream(System.`in`)))
}
