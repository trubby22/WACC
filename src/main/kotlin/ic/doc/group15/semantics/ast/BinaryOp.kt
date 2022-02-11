package ic.doc.group15.semantics.ast

import ic.doc.group15.semantics.BasicType.*
import ic.doc.group15.semantics.ReturnableType
import ic.doc.group15.semantics.SymbolTable

private val intOnly = setOf<ReturnableType>(IntType)
private val comparables = setOf<ReturnableType>(IntType, CharType)
private val boolOnly = setOf<ReturnableType>(BoolType)

enum class BinaryOp(
    private val allowedTypes: Set<ReturnableType>? = intOnly,
    val returnType: ReturnableType = BoolType
) {
    MULT(returnType = IntType),
    DIV(returnType = IntType),
    MOD(returnType = IntType),
    PLUS(returnType = IntType),
    MINUS(returnType = IntType),
    GT(comparables),
    GTE(comparables),
    LT(comparables),
    LTE(comparables),
    EQUALS(null),
    NOT_EQUALS(null),
    AND(boolOnly),
    OR(boolOnly);

    fun generateNode(
        st: SymbolTable,
        expr1: ExpressionAST,
        expr2: ExpressionAST
    ): BinaryOpExprAST {
        if (!expr1.type.compatible(expr2.type)) {
            throw TypeError(
                "operands of binary operation expressions must have the same type"
            )
        } else if (allowedTypes != null && !allowedTypes.contains(expr1.type)) {
            throw TypeError(
                "cannot apply $this to arguments of type ${expr1.type}"
            )
        }
        return BinaryOpExprAST(st, expr1, expr2, this)
    }
}
