package ic.doc.group15

import ic.doc.group15.antlr.BasicLexer
import ic.doc.group15.antlr.BasicParser
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream

fun main() {
    val input = CharStreams.fromStream(System.`in`)
    val lexer = BasicLexer(input)
    val tokens = CommonTokenStream(lexer)
    val parser = BasicParser(tokens)
    val tree = parser.prog()

    println(tree.toStringTree(parser))
}
