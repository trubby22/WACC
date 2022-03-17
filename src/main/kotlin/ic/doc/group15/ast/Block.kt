package ic.doc.group15.ast

import ic.doc.group15.SymbolTable
import ic.doc.group15.assembly.BranchLabel
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
    val paramSymbolTable: SymbolTable,
    symbolTable: SymbolTable,
    val returnType: Type,
    val funcName: String
) : BlockAST(parent, symbolTable) {

    lateinit var funcIdent: FunctionType

    val formals: MutableList<ParameterAST> = mutableListOf()

    var returnStat: ReturnStatementAST? = null
}

class IfBlockAST(
    parent: BlockAST,
    symbolTable: SymbolTable,
    val condExpr: ExpressionAST
) : BlockAST(parent, symbolTable) {

    lateinit var elseBlock: ElseBlockAST
}

class ElseBlockAST(
    parent: IfBlockAST,
    symbolTable: SymbolTable
) : BlockAST(parent, symbolTable)

class WhileBlockAST(
    parent: BlockAST,
    symbolTable: SymbolTable,
    val condExpr: ExpressionAST
) : BlockAST(parent, symbolTable) {
    lateinit var checkLabel : BranchLabel
    lateinit var endLabel : BranchLabel
}


class ForBlockOuterScopeAST(
    parent: BlockAST,
    symbolTable: SymbolTable) : BlockAST(parent, symbolTable) {
    lateinit var forBlock: ForBlockAST
    lateinit var varDecl: VariableDeclarationAST
}


class ForBlockAST(
    parent: BlockAST,
    symbolTable: SymbolTable
) : BlockAST(parent, symbolTable) {
    lateinit var condExpr: ExpressionAST
    lateinit var loopVarUpdate: StatementAST
    lateinit var loopVarUpdateLabel : BranchLabel
    lateinit var endLabel : BranchLabel
}

class ForInRangeBlockOuterScopeAST(
    parent: BlockAST,
    symbolTable: SymbolTable) : BlockAST(parent, symbolTable) {
    lateinit var forInRangeBlock: ForInRangeBlockAST
    lateinit var varDecl: VariableDeclarationAST
}

class ForInRangeBlockAST(
    parent: BlockAST,
    symbolTable: SymbolTable
) : BlockAST(parent, symbolTable) {
    lateinit var condExpr: ExpressionAST
    lateinit var loopVarIncrementStat: StatementAST
    lateinit var loopVarIncrementLabel : BranchLabel
    lateinit var endLabel : BranchLabel
}

open class BeginEndBlockAST(
    parent: BlockAST?,
    symbolTable: SymbolTable
) : BlockAST(parent, symbolTable)
