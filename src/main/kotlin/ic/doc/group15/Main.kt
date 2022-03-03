package ic.doc.group15

import ic.doc.group15.antlr.WaccLexer
import ic.doc.group15.antlr.WaccParser
import ic.doc.group15.ast.AST
import ic.doc.group15.codegen.AssemblyGenerator
import ic.doc.group15.error.SemanticErrorList
import ic.doc.group15.error.syntactic.SyntacticErrorListener
import ic.doc.group15.visitor.Visitor
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
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
    val semanticErrors = SemanticErrorList()
    val visitor = Visitor(ast, st, semanticErrors, enableLogging = true)
    visitor.visit(program)

    semanticErrors.checkErrors()

    val assemblyGenerator = AssemblyGenerator(ast)
    val assemblyCode = assemblyGenerator.generate()

    // Create assembly file
    val filename = args[0].split("/").last()
    val asmFilename = filename.substring(0, filename.length - 4) + "s"

    File(asmFilename).writeText(assemblyCode)
}
