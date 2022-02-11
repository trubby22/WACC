package ic.doc.group15.semantics.ast

import ic.doc.group15.semantics.ASTNode
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

class VariableAssignmentAST(
    parent: BlockAST,
    symbolTable: SymbolTable,
    val lhs: ExpressionAST,
    val rhs: ASTNode
) : StatementAST(parent, symbolTable) {

    lateinit var varIdent: Variable
}

class ReadStatementAST(
    parent: BlockAST,
    symbolTable: SymbolTable,
    val varName: String
) : StatementAST(parent, symbolTable) {

    lateinit var varIdent: Variable
}

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

class SequenceStatementAST(
    parent: BlockAST,
    symbolTable: SymbolTable,
    val stat1: StatementAST,
    val stat2: StatementAST
) : StatementAST(parent, symbolTable)
