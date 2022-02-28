package ic.doc.group15.ast

import ic.doc.group15.SymbolTable
import ic.doc.group15.type.*
import java.util.*

abstract class BlockAST protected constructor(
    parent: BlockAST?,
    symbolTable: SymbolTable = SymbolTable.emptyTable
) : StatementAST(parent, symbolTable) {

    val statements: MutableList<StatementAST> = LinkedList()
}

class FunctionDeclarationAST(
    parent: BlockAST,
    symbolTable: SymbolTable,
    val returnType: Type,
    val funcName: String,
) : BlockAST(parent, symbolTable) {

    lateinit var funcIdent: FunctionType

    val formals: MutableList<ParameterAST> = mutableListOf()

    var returnStat: ReturnStatementAST? = null
}

class IfBlockAST(
    parent: BlockAST,
    symbolTable: SymbolTable,
    val condExpr: ExpressionAST,
    val elseStat: ElseBlockAST?
) : BlockAST(parent, symbolTable)

class ElseBlockAST(
    parent: IfBlockAST,
    symbolTable: SymbolTable
) : BlockAST(parent, symbolTable)

class WhileBlockAST(
    parent: BlockAST,
    symbolTable: SymbolTable,
    val condExpr: ExpressionAST
) : BlockAST(parent, symbolTable)

open class BeginEndBlockAST(
    parent: BlockAST?,
    symbolTable: SymbolTable
) : BlockAST(parent, symbolTable)
