package ic.doc.group15.semantics.ast

import ic.doc.group15.semantics.Param
import ic.doc.group15.semantics.SymbolTable
import ic.doc.group15.semantics.Variable

abstract class StatementAST protected constructor(
    val parent: BlockAST?,
    symbolTable: SymbolTable = SymbolTable.emptyTable
) : ASTNode(symbolTable)

open class VariableDeclarationAST(
    parent: BlockAST,
    symbolTable: SymbolTable,
    val typeName: String,
    val varName: String
) : StatementAST(parent, symbolTable) {

    lateinit var varIdent: Variable
}

class ParameterAST(
    parent: BlockAST,
    symbolTable: SymbolTable,
    typeName: String,
    paramName: String
) : VariableDeclarationAST(parent, symbolTable, typeName, paramName) {

    lateinit var paramIdent: Param
}

class ReadStatementAST(
    parent: BlockAST,
    symbolTable: SymbolTable,
    val target: AssignmentAST
) : StatementAST(parent, symbolTable)

class SkipStatementAST(
    parent: BlockAST,
) : StatementAST(parent)

class FreeStatementAST(
    parent: BlockAST,
    symbolTable: SymbolTable,
    val expr: ExpressionAST
) : StatementAST(parent, symbolTable)

class ReturnStatementAST(
    parent: BlockAST,
    symbolTable: SymbolTable,
    val expr: ExpressionAST
) : StatementAST(parent, symbolTable)

class ExitStatementAST(
    parent: BlockAST,
    val expr: ExpressionAST
) : StatementAST(parent)

class PrintStatementAST(
    parent: BlockAST,
    symbolTable: SymbolTable,
    val expr: ExpressionAST
) : StatementAST(parent, symbolTable)

class PrintlnStatementAST(
    parent: BlockAST,
    symbolTable: SymbolTable,
    val expr: ExpressionAST
) : StatementAST(parent, symbolTable)
