package ic.doc.group15.ast

import ic.doc.group15.SymbolTable
import ic.doc.group15.type.*

abstract class StatementAST protected constructor(
    val parent: BlockAST?,
    symbolTable: SymbolTable = SymbolTable.emptyTable
) : ASTNode(symbolTable)

open class VariableDeclarationAST(
    parent: BlockAST,
    symbolTable: SymbolTable,
    val varName: String,
    val rhs: AssignRhsAST,
    val varIdent: Variable
) : StatementAST(parent, symbolTable)

class ParameterAST(
    symbolTable: SymbolTable,
    paramName: String,
    override val ident: Param
) : VariableIdentifierAST(symbolTable, paramName, ident)

class ReadStatementAST(
    parent: BlockAST,
    symbolTable: SymbolTable,
    val target: AssignmentAST<*>
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