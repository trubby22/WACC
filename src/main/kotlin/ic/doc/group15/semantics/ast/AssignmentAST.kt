package ic.doc.group15.semantics.ast

import ic.doc.group15.semantics.*
import java.util.*

abstract class AssignRhsAST protected constructor(
    symbolTable: SymbolTable = SymbolTable.emptyTable,
    val type: Type
) : ASTNode(symbolTable)

abstract class AssignmentAST protected constructor(
    parent: BlockAST,
    val lhs: ASTNode,
    val rhs: AssignRhsAST
) : StatementAST(parent)

class AssignToIdentAST(
    parent: BlockAST,
    lhs: VariableIdentifierAST,
    rhs: AssignRhsAST
) : AssignmentAST(parent, lhs, rhs)

class AssignToArrayElemAST(
    parent: BlockAST,
    lhs: ArrayElemAST,
    rhs: AssignRhsAST
) : AssignmentAST(parent, lhs, rhs)

class AssignToPairElemAST(
    parent: BlockAST,
    lhs: PairElemAST,
    rhs: AssignRhsAST
) : AssignmentAST(parent, lhs, rhs)

class ArrayLiteralAST(
    elemType: Type,
    val elems: List<ExpressionAST>
) : AssignRhsAST(type = ArrayType(elemType, elems.size))

class NewPairAST(
    symbolTable: SymbolTable,
    val fstExpr: ExpressionAST,
    val sndExpr: ExpressionAST
) : AssignRhsAST(symbolTable, type = PairType(fstExpr.type, sndExpr.type))

abstract class PairElemAST protected constructor(
    symbolTable: SymbolTable,
    val expr: ExpressionAST,
    val elemType: Type
) : AssignRhsAST(symbolTable, elemType)

class FstPairElemAST(
    symbolTable: SymbolTable,
    val expr: ExpressionAST
) : AssignRhsAST(symbolTable, (expr.type as PairType).leftType)

class SndPairElemAST(
    symbolTable: SymbolTable,
    val expr: ExpressionAST
) : AssignRhsAST(symbolTable, (expr.type as PairType).rightType)

class CallAST(
    symbolTable: SymbolTable,
    val funcName: String,
    val funcIdent: FunctionType
) : AssignRhsAST(symbolTable, funcIdent.returnType) {

    val actuals: MutableList<ExpressionAST> = LinkedList()
}
