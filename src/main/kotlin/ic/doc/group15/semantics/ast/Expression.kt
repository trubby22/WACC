package ic.doc.group15.semantics.ast

import ic.doc.group15.semantics.*

abstract class ExpressionAST protected constructor(
    symbolTable: SymbolTable = SymbolTable.emptyTable,
    type: Type
) : AssignRhsAST(symbolTable, type)

class IntLiteralAST(val intValue: Int) : ExpressionAST(type = BasicType.IntType)
class BoolLiteralAST(val boolValue: Boolean) : ExpressionAST(type = BasicType.BoolType)
class CharLiteralAST(val charValue: Char) : ExpressionAST(type = BasicType.CharType)
class StringLiteralAST(val stringValue: String) : ExpressionAST(type = BasicType.StringType)

class NullPairLiteralAST : ExpressionAST(type = PairType())

class VariableIdentifierAST(
    symbolTable: SymbolTable,
    val varName: String,
    val varIdent: Variable
) : ExpressionAST(symbolTable, type = varIdent.type)

class ArrayElemAST(
    symbolTable: SymbolTable,
    val indexExp: ExpressionAST,
    val arrayName: String,
    val arrType: Type
) : ExpressionAST(symbolTable, arrType)

class UnaryOpExprAST(
    symbolTable: SymbolTable,
    val expr: ExpressionAST,
    val operator: UnaryOp
) : ExpressionAST(symbolTable, type = operator.returnType)

class BinaryOpExprAST(
    symbolTable: SymbolTable,
    val expr1: ExpressionAST,
    val expr2: ExpressionAST,
    val operator: BinaryOp
) : ExpressionAST(symbolTable, type = operator.returnType)
