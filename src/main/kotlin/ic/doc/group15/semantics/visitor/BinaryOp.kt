package ic.doc.group15.semantics.visitor

import ic.doc.group15.antlr.WaccParser.*
import ic.doc.group15.semantics.BasicType
import ic.doc.group15.semantics.ReturnableType
import ic.doc.group15.semantics.SymbolTable
import ic.doc.group15.semantics.TypeError
import ic.doc.group15.semantics.ast.BinaryOpExprAST
import ic.doc.group15.semantics.ast.ExpressionAST
import kotlin.reflect.KClass

private val intOnly = setOf<ReturnableType>(BasicType.IntType)
private val comparables = setOf<ReturnableType>(BasicType.IntType, BasicType.CharType)
private val boolOnly = setOf<ReturnableType>(BasicType.BoolType)

enum class BinaryOp(
    private val opClass: KClass<out Binary_opContext>,
    private val allowedTypes: Set<ReturnableType>? = intOnly
) {
    MULT(MultBinaryOpContext::class),
    DIV(DivBinaryOpContext::class),
    MOD(ModBinaryOpContext::class),
    PLUS(PlusBinaryOpContext::class),
    MINUS(MinusBinaryOpContext::class),
    GT(GtBinaryOpContext::class, comparables),
    GTE(GteBinaryOpContext::class, comparables),
    LT(LtBinaryOpContext::class, comparables),
    LTE(LteBinaryOpContext::class, comparables),
    EQUALS(EqualsBinaryOpContext::class, null),
    NOT_EQUALS(NotEqualsBinaryOpContext::class, null),
    AND(AndBinaryOpContext::class, boolOnly),
    OR(OrBinaryOpContext::class, allowedTypes = boolOnly);

    companion object {
        private val binOpMap = BinaryOp.values().associateBy(BinaryOp::opClass)

        fun generateNode(
            st: SymbolTable,
            expr1: ExpressionAST,
            expr2: ExpressionAST,
            ctx: Binary_opContext
        ): BinaryOpExprAST {
            val binOp = binOpMap[ctx::class]
            assert(binOp != null)
            if (!expr1.type.compatible(expr2.type)) {
                throw TypeError(
                    "operands of binary operation expressions must have the same type"
                )
            } else if (binOp!!.allowedTypes != null && !binOp.allowedTypes!!.contains(expr1.type)) {
                throw TypeError(
                    "cannot apply $binOp to arguments of type ${expr1.type}"
                )
            }
            return BinaryOpExprAST(st, expr1, expr2, binOp)
        }
    }
}
