package ic.doc.group15.ast

import ic.doc.group15.SymbolTable
import ic.doc.group15.type.*
import java.util.*

abstract class AssignRhsAST protected constructor(
    symbolTable: SymbolTable = SymbolTable.emptyTable,
    val type: Type
) : ASTNode(symbolTable)

abstract class AssignmentAST protected constructor(
    parent: BlockAST,
    val lhs: ASTNode,
    val type: Type,
) : StatementAST(parent) {
    lateinit var rhs: AssignRhsAST
}

class AssignToIdentAST(
    parent: BlockAST,
    lhs: VariableIdentifierAST,
) : AssignmentAST(parent, lhs, lhs.type)

class AssignToArrayElemAST(
    parent: BlockAST,
    lhs: ArrayElemAST,
) : AssignmentAST(parent, lhs, lhs.type)

class AssignToPairElemAST(
    parent: BlockAST,
    lhs: PairElemAST
) : AssignmentAST(parent, lhs, lhs.type)

class ArrayLiteralAST(
    symbolTable: SymbolTable,
    elemType: Type,
    val elems: List<ExpressionAST>
) : AssignRhsAST(symbolTable, ArrayType(elemType, 1))

class NewPairAST(
    symbolTable: SymbolTable,
    val fstExpr: ExpressionAST,
    val sndExpr: ExpressionAST
) : AssignRhsAST(symbolTable, PairType(fstExpr.type, sndExpr.type))

abstract class PairElemAST protected constructor(
    symbolTable: SymbolTable,
    val elemType: Type
) : AssignRhsAST(symbolTable, elemType)

class FstPairElemAST(
    symbolTable: SymbolTable,
    val pairExpr: ExpressionAST
) : PairElemAST(symbolTable, (pairExpr.type as PairType).fstType)

class SndPairElemAST(
    symbolTable: SymbolTable,
    val pairExpr: ExpressionAST
) : PairElemAST(symbolTable, (pairExpr.type as PairType).sndType)

class CallAST(
    symbolTable: SymbolTable,
    val funcName: String,
    val funcIdent: FunctionType
) : AssignRhsAST(symbolTable, funcIdent.returnType) {

    val actuals: MutableList<ExpressionAST> = LinkedList()
}
