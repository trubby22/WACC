package ic.doc.group15.semantics.ast

import ic.doc.group15.semantics.BasicType
import ic.doc.group15.semantics.ReturnableType
import ic.doc.group15.semantics.SymbolTable
import ic.doc.group15.semantics.TypeError

private val intOnly = setOf<ReturnableType>(BasicType.IntType)
private val comparables = setOf<ReturnableType>(BasicType.IntType, BasicType.CharType)
private val boolOnly = setOf<ReturnableType>(BasicType.BoolType)

enum class BinaryOp(
    private val allowedTypes: Set<ReturnableType>? = intOnly
) {
    MULT,
    DIV,
    MOD,
    PLUS,
    MINUS,
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
        } else if (allowedTypes != null && !allowedTypes!!.contains(expr1.type)) {
            throw TypeError(
                "cannot apply $this to arguments of type ${expr1.type}"
            )
        }
        return BinaryOpExprAST(st, expr1, expr2, this)
    }
}
