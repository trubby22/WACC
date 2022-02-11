package ic.doc.group15

import ic.doc.group15.antlr.WaccLexer
import ic.doc.group15.antlr.WaccParser
import ic.doc.group15.semantics.SymbolTable
import ic.doc.group15.semantics.Visitor
import ic.doc.group15.semantics.ast.AST
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream

fun main() {
    val input = CharStreams.fromStream(System.`in`)

    val lexer = WaccLexer(input)
    val tokens = CommonTokenStream(lexer)
    val parser = WaccParser(tokens)
    parser.removeErrorListeners()

    val program = parser.program()

    println(program.toStringTree(parser))

    val st = SymbolTable.topLevel()
    val ast = AST(st)
    val visitor = Visitor(ast, st)
    visitor.visit(program)
}
