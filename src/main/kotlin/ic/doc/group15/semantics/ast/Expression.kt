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
    val varIdent: Variable
) : ExpressionAST(type = varIdent.type)

class ArrayElemAST(
    symbolTable: SymbolTable,
    val indexExp: ExpressionAST,
    val arrayName: String,
    val varIdent: Variable
) : ExpressionAST(symbolTable, (varIdent.type as ArrayType).elementType)

class UnaryOpAST(
    symbolTable: SymbolTable,
    val unOpExp: String,
    val expr: ExpressionAST,
    val unOpType: Type
) : ExpressionAST(symbolTable, unOpType) {
    fun check() {
        // expr.check()
    }
}

class BinaryOpAST(
    symbolTable: SymbolTable,
    val binOpExp: String,
    val expr1: ExpressionAST,
    val expr2: ExpressionAST,
    val binOpType: Type
) : ExpressionAST(symbolTable, binOpType) {
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
