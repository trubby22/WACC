package ic.doc.group15.semantics

import java.util.LinkedList

abstract class ASTNode protected constructor(
    val parent: ASTNode?,
    val symbolTable: SymbolTable = SymbolTable.emptyTable
)

class AST(topLevelSymbolTable: SymbolTable) : BeginEndBlockAST(null, topLevelSymbolTable)

abstract class ExpressionAST protected constructor(
    parent: ASTNode,
    symbolTable: SymbolTable = SymbolTable.emptyTable
) : ASTNode(parent, symbolTable) {

    lateinit var type: Type
}

class IntLiteralAST(
    parent: ASTNode,
    val intValue: Int
) : ExpressionAST(parent)

class BoolLiteralAST(
    parent: ASTNode,
    val boolValue: Int
) : ExpressionAST(parent)

class CharLiteralAST(
    parent: ASTNode,
    val charValue: Char
) : ExpressionAST(parent)

class StringLiteralAST(
    parent: ASTNode,
    val stringValue: String
) : ExpressionAST(parent)

class NullPairLiteralAST(
    parent: ASTNode
) : ExpressionAST(parent)

class VariableExpressionAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val varName: String
) : ExpressionAST(parent, symbolTable) {

    lateinit var varIdent: Variable
}

class UnaryOpAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val unOpExp: String,
    val expr: ExpressionAST,
    val unOpType: Type
) : ExpressionAST(parent, symbolTable) {
    fun check() {
        // expr.check()
    }
}

class BinaryOpAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val binOpExp: String,
    val expr1: ExpressionAST,
    val expr2: ExpressionAST,
    val binOpType: Type
) : ExpressionAST(parent, symbolTable) {
    fun check() {
        // expr1.check()
        // expr2.check()
//        if (expr1.type != expr2.type) {
//            throw TypeError("Operands must be of the same type\n")
//        }
    }
}

abstract class StatementAST protected constructor(
    parent: ASTNode?,
    symbolTable: SymbolTable = SymbolTable.emptyTable
) : ASTNode(parent, symbolTable)

abstract class BlockAST protected constructor(
    parent: ASTNode?,
    symbolTable: SymbolTable = SymbolTable.emptyTable
) : StatementAST(parent, symbolTable) {

    val statements: MutableList<StatementAST> = LinkedList()
}

open class VariableDeclarationAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val typeName: String,
    val varName: String
) : ASTNode(parent, symbolTable) {

    lateinit var varIdent: Variable
}

class VariableAssignmentAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val varname: String,
    val expr: ExpressionAST
) : ASTNode(parent, symbolTable) {

    lateinit var varIdent: Variable
}

class ArrayElemAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val indexExp: ExpressionAST,
    val arrayName: String,
    val arrayType: ArrayType
) : ASTNode(parent, symbolTable)

class ParameterAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    typeName: String,
    val paramName: String
) : VariableDeclarationAST(parent, symbolTable, typeName, paramName) {

    lateinit var paramIdent: Param
}

class FunctionDeclarationAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val returnTypeName: String,
    val funcName: String,
) : BlockAST(parent, symbolTable) {

    lateinit var funcIdent: FunctionType

    val formals: MutableList<ParameterAST> = mutableListOf()

    var returnStat: ReturnStatementAST? = null
}

class CallAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val funcName: String,
) : ASTNode(parent, symbolTable) {

    lateinit var funcIdent: FunctionType

    val actuals: MutableList<ExpressionAST> = LinkedList()
}

class ReadStatementAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val varName: String
) : StatementAST(parent, symbolTable) {

    lateinit var varIdent: Variable
}

class SkipStatementAST(
    parent: ASTNode,
) : StatementAST(parent)

class FreeStatementAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val expr: ExpressionAST
) : StatementAST(parent, symbolTable)

class ReturnStatementAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val expr: ExpressionAST
) : StatementAST(parent, symbolTable)

class ExitStatementAST(
    parent: ASTNode,
    val expr: ExpressionAST
) : StatementAST(parent)

class PrintStatementAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val expr: ExpressionAST
) : StatementAST(parent, symbolTable)

class IfBlockAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val condExpr: ExpressionAST,
    val thenStat: StatementAST,
    val elseStat: StatementAST
) : BlockAST(parent, symbolTable)

class WhileBlockAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val condExpr: ExpressionAST
) : BlockAST(parent, symbolTable)

open class BeginEndBlockAST(
    parent: ASTNode?,
    symbolTable: SymbolTable
) : BlockAST(parent, symbolTable)
