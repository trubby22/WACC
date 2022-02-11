package ic.doc.group15.semantics.ast

import ic.doc.group15.semantics.*
import ic.doc.group15.semantics.visitor.BinaryOp
import ic.doc.group15.semantics.visitor.UnaryOp
import java.util.LinkedList

abstract class ExpressionAST protected constructor(
    symbolTable: SymbolTable = SymbolTable.emptyTable,
    val type: Type
) : ASTNode(symbolTable)

class IntLiteralAST(val intValue: Int) : ExpressionAST(type = BasicType.IntType)
class BoolLiteralAST(val boolValue: Boolean) : ExpressionAST(type = BasicType.BoolType)
class CharLiteralAST(val charValue: Char) : ExpressionAST(type = BasicType.CharType)
class StringLiteralAST(val stringValue: String) : ExpressionAST(type = BasicType.StringType)

class NullPairLiteralAST : ExpressionAST(type = PairType(null, null))

class ArrayLiteralAST(val elemType: Type) : ExpressionAST(type = elemType) {
    val elems: MutableList<ExpressionAST> = LinkedList()
}

class VariableIdentifierAST(
    symbolTable: SymbolTable,
    val varName: String,
    val varIdent: Variable
) : ExpressionAST(type = varIdent.type)

class ArrayElemAST(
    symbolTable: SymbolTable,
    val firstIndexExp: ExpressionAST,
    val arrayName: String,
    val arrType: Type
) : ExpressionAST(symbolTable, arrType)

class PairAST(
    symbolTable: SymbolTable,
    val fst: ExpressionAST,
    val snd: ExpressionAST
) : ExpressionAST(symbolTable, type = PairType(fst.type, snd.type))

class UnaryOpExprAST(
    symbolTable: SymbolTable,
    val expr: ExpressionAST,
    val operator: UnaryOp
) : ExpressionAST(symbolTable, type = expr.type)

class BinaryOpExprAST(
    symbolTable: SymbolTable,
    val expr1: ExpressionAST,
    val expr2: ExpressionAST,
    val operator: BinaryOp,
    val exprType: Type
) : ExpressionAST(symbolTable, type = exprType)

class CallAST(
    symbolTable: SymbolTable,
    val funcName: String,
) : ASTNode(symbolTable) {

    lateinit var funcIdent: FunctionType

    val actuals: MutableList<ExpressionAST> = LinkedList()
}
