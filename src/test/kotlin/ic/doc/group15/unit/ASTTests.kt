package ic.doc.group15.unit

//import ic.doc.group15.antlr.WaccLexer
//import ic.doc.group15.antlr.WaccParser
//import ic.doc.group15.semantics.SymbolTable
//import ic.doc.group15.semantics.Visitor
//import ic.doc.group15.semantics.ast.*
//import org.antlr.v4.runtime.CharStream
//import org.antlr.v4.runtime.CharStreams;
//import org.antlr.v4.runtime.CommonTokenStream;
//import org.junit.jupiter.api.Assertions.assertTrue
//import org.junit.jupiter.api.Test
//
//public class ASTTests {
//
//    fun getProgram(filepath: String): AST {
//        val input = CharStreams.fromFileName(filepath)
//        val lexer = WaccLexer(input)
//        val tokens = CommonTokenStream(lexer)
//        val parser = WaccParser(tokens)
//        val program = parser.program()
//        val st = SymbolTable.topLevel()
//        val ast = AST(st)
//        val visitor = Visitor(ast, st)
//        visitor.visit(program)
//        return ast;
//    }
//
//    @Test
//    fun test1() {
//        // depth first
//        val program = getProgram("wacc_examples/valid/if/if1.wacc")
//        assertTrue(program.parent == null)
//        assertTrue(program.statements.size == 2)
//        assertTrue(program.statements[0] is VariableDeclarationAST)
//        val stat1: VariableDeclarationAST = program.statements[0] as VariableDeclarationAST
//        // assertTrue(stat1.assRhs is IntLiteralAST)
//        assertTrue(program.statements[1] is IfBlockAST)
//        val stat2: IfBlockAST = program.statements[1] as IfBlockAST
//        assertTrue(stat2.condExpr is BinaryOpExprAST)
//        val stat21: BinaryOpExprAST = stat2.condExpr as BinaryOpExprAST
//        assertTrue(stat21.expr1 is VariableIdentifierAST)
//        assertTrue(stat21.expr2 is IntLiteralAST)
//        assertTrue(stat2.thenStat is PrintStatementAST)
//        val stat22: PrintStatementAST = stat2.thenStat as PrintStatementAST
//        assertTrue(stat22.expr is StringLiteralAST)
//        assertTrue(stat2.elseStat is PrintStatementAST)
//        val stat23: PrintStatementAST = stat2.elseStat as PrintStatementAST
//        assertTrue(stat23.expr is StringLiteralAST)
//    }
//
//    @Test
//    fun test2() {
//        val program = getProgram("wacc_examples/valid/while/whileCount.wacc")
//        assertTrue(program.parent == null)
//        assertTrue(program.statements.size == 3)
//        assertTrue(program.statements[0] is VariableDeclarationAST)
//        val stat1: VariableDeclarationAST = program.statements[0] as VariableDeclarationAST
//        // assertTrue(stat1.assRhs is IntLiteralAST)
//        assertTrue(program.statements[1] is PrintStatementAST)
//        val stat2: PrintStatementAST = program.statements[1] as PrintStatementAST
//        assertTrue(stat2.expr is StringLiteralAST)
//        assertTrue(program.statements[2] is WhileBlockAST)
//        val stat3: WhileBlockAST = program.statements[2] as WhileBlockAST
//        assertTrue(stat3.condExpr is BinaryOpExprAST)
//        val stat31: BinaryOpExprAST = stat3.condExpr as BinaryOpExprAST
//        assertTrue(stat31.expr1 is VariableIdentifierAST)
//        assertTrue(stat31.expr2 is IntLiteralAST)
//        assertTrue(stat3.statements.size == 2)
//        assertTrue(stat3.statements[0] is PrintStatementAST)
//        val stat32: PrintStatementAST = stat3.statements[0] as PrintStatementAST
//        assertTrue(stat32.expr is StringLiteralAST)
//        assertTrue(stat3.statements[1] is VariableAssignmentAST)
//        val stat33: VariableAssignmentAST = stat3.statements[1] as VariableAssignmentAST
//        assertTrue(stat33.lhs is VariableIdentifierAST)
//        assertTrue(stat33.rhs is BinaryOpExprAST)
//        val stat331: BinaryOpExprAST = stat33.rhs as BinaryOpExprAST
//        assertTrue(stat331.expr1 is VariableIdentifierAST)
//        assertTrue(stat331.expr2 is IntLiteralAST)
//    }
//}