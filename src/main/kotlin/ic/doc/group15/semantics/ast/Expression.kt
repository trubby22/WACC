package ic.doc.group15.semantics.ast

import ic.doc.group15.semantics.*
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
    val varType: Type
) : ExpressionAST(type = varType)

class ArrayElemAST(
    symbolTable: SymbolTable,
    val indexExp: ExpressionAST,
    val arrayName: String,
    val varIdent: Variable
) : ExpressionAST(symbolTable, (varIdent.type as ArrayType).elementType)

class UnaryOpAST(
    symbolTable: SymbolTable,
    val operator: UnaryOp
) : ASTNode(symbolTable)

class UnaryOpExprAST(
    symbolTable: SymbolTable,
    val expr: ExpressionAST,
    val operator: UnaryOpAST
) : ExpressionAST(symbolTable, type = expr.type) {
    fun check() {
        // expr.check()
    }
}

class BinaryOpAST(
    symbolTable: SymbolTable,
    val operator: BinaryOp
) : ASTNode(symbolTable)

class BinaryOpExprAST(
    symbolTable: SymbolTable,
    val expr1: ExpressionAST,
    val expr2: ExpressionAST,
    val operator: BinaryOpAST
) : ExpressionAST(symbolTable, type = expr1.type) {
    fun check() {
        // expr1.check()
        // expr2.check()
//        if (expr1.type != expr2.type) {
//            throw TypeError("Operands must be of the same type\n")
//        }
    }
}

class CallAST(
    symbolTable: SymbolTable,
    val funcName: String,
) : ASTNode(symbolTable) {

    lateinit var funcIdent: FunctionType

    val actuals: MutableList<ExpressionAST> = LinkedList()
}
