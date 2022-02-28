package ic.doc.group15.ast

import ic.doc.group15.SymbolTable
import ic.doc.group15.type.*

abstract class ExpressionAST protected constructor(
    symbolTable: SymbolTable = SymbolTable.emptyTable,
    type: Type
) : AssignRhsAST(symbolTable, type)

class IntLiteralAST(val intValue: Int) : ExpressionAST(type = BasicType.IntType)
class BoolLiteralAST(val boolValue: Boolean) : ExpressionAST(type = BasicType.BoolType)
class CharLiteralAST(val charValue: Char) : ExpressionAST(type = BasicType.CharType)
class StringLiteralAST(val stringValue: String) : ExpressionAST(type = BasicType.StringType)
class NullPairLiteralAST : ExpressionAST(type = PairType())

open class VariableIdentifierAST(
    symbolTable: SymbolTable,
    val varName: String,
    open val ident: Variable
) : ExpressionAST(symbolTable, ident.type)

class ArrayElemAST(
    symbolTable: SymbolTable,
    val arrayName: String,
    val indexExpr: List<ExpressionAST>,
    val elemType: Type
) : ExpressionAST(symbolTable, elemType)

class UnaryOpExprAST(
    symbolTable: SymbolTable,
    val expr: ExpressionAST,
    val operator: UnaryOp
) : ExpressionAST(symbolTable, operator.returnType)

class BinaryOpExprAST(
    symbolTable: SymbolTable,
    val expr1: ExpressionAST,
    val expr2: ExpressionAST,
    val operator: BinaryOp
) : ExpressionAST(symbolTable, operator.returnType)