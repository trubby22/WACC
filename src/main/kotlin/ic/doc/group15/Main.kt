package ic.doc.group15

import ic.doc.group15.antlr.WaccLexer
import ic.doc.group15.antlr.WaccParser
import ic.doc.group15.semantics.SymbolTable
import ic.doc.group15.semantics.Visitor
import ic.doc.group15.semantics.ast.AST
import ic.doc.group15.semantics.ast.SemanticError
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import kotlin.system.exitProcess
import java.io.File

fun main(args: Array<String>) {
    assert(args.size == 1)
    val input = CharStreams.fromStream(System.`in`)

    val lexer = WaccLexer(input)
    val tokens = CommonTokenStream(lexer)
    val parser = WaccParser(tokens)
    val syntacticErrorListener = SyntacticErrorListener()
    parser.addErrorListener(syntacticErrorListener)

    val program = parser.program()

    syntacticErrorListener.terminateIfSyntacticErrorOccurred()
    parser.removeErrorListener(syntacticErrorListener)

    val st = SymbolTable.topLevel()
    val ast = AST(st)
    val visitor = Visitor(ast, st)
    try {
        visitor.visit(program)
    } catch (e: SemanticError) {
        println(e.message)
        exitProcess(200)
    }

//    Create a dummy assembly file
    val filename = args[0].split("/").last()
    val asmFilename = filename.substring(0, filename.length - 4) + "s"

    File(asmFilename).writeText(".text\n" +
            "\n" +
            ".global main\n" +
            "main:\n" +
            "       PUSH {lr}\n" +
            "       LDR r0, =0\n" +
            "       POP {pc}\n" +
            "       .ltorg")
}
