package ic.doc.group15.ast

import ic.doc.group15.SymbolTable
import ic.doc.group15.type.*
import java.util.*

abstract class AssignRhsAST protected constructor(
    symbolTable: SymbolTable = SymbolTable.emptyTable,
    val type: ReturnableType
) : ASTNode(symbolTable)

abstract class AssignmentAST<T : ASTNode> protected constructor(
    parent: BlockAST,
    val lhs: T,
    val type: ReturnableType,
) : StatementAST(parent) {
    lateinit var rhs: AssignRhsAST
}

class AssignToIdentAST(
    parent: BlockAST,
    lhs: VariableIdentifierAST,
) : AssignmentAST<VariableIdentifierAST>(parent, lhs, lhs.type)

class AssignToArrayElemAST(
    parent: BlockAST,
    lhs: ArrayElemAST,
) : AssignmentAST<ArrayElemAST>(parent, lhs, lhs.type)

class AssignToPairElemAST(
    parent: BlockAST,
    lhs: PairElemAST
) : AssignmentAST<PairElemAST>(parent, lhs, lhs.type)

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
    val elemType: ReturnableType
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
    val funcIdent: FunctionType,
    val actuals: MutableList<ExpressionAST>
) : AssignRhsAST(symbolTable, funcIdent.returnType)

