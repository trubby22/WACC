package ic.doc.group15.semantics.visitor

import ic.doc.group15.antlr.WaccParser.*
import ic.doc.group15.semantics.*
import ic.doc.group15.semantics.ast.BinaryOpExprAST
import ic.doc.group15.semantics.ast.ExpressionAST
import kotlin.reflect.KClass

private val intOnly = setOf<ReturnableType>(BasicType.IntType)
private val comparables = setOf<ReturnableType>(BasicType.IntType, BasicType.CharType)
private val boolOnly = setOf<ReturnableType>(BasicType.BoolType)

enum class BinaryOp(
    private val opClass: KClass<out Binary_opContext>,
    private val allowedTypes: Set<ReturnableType>? = intOnly,
    private val returnType: Type
) {
    MULT(MultBinaryOpContext::class, returnType = BasicType.IntType),
    DIV(DivBinaryOpContext::class, returnType = BasicType.IntType),
    MOD(ModBinaryOpContext::class, returnType = BasicType.IntType),
    PLUS(PlusBinaryOpContext::class, returnType = BasicType.IntType),
    MINUS(MinusBinaryOpContext::class, returnType = BasicType.IntType),
    GT(GtBinaryOpContext::class, comparables, returnType = BasicType.BoolType),
    GTE(GteBinaryOpContext::class, comparables, returnType = BasicType.BoolType),
    LT(LtBinaryOpContext::class, comparables, returnType = BasicType.BoolType),
    LTE(LteBinaryOpContext::class, comparables, returnType = BasicType.BoolType),
    EQUALS(EqualsBinaryOpContext::class, null, returnType = BasicType.BoolType),
    NOT_EQUALS(NotEqualsBinaryOpContext::class, null, returnType = BasicType.BoolType),
    AND(AndBinaryOpContext::class, boolOnly, returnType = BasicType.BoolType),
    OR(OrBinaryOpContext::class, allowedTypes = boolOnly, returnType = BasicType.BoolType);

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

            return BinaryOpExprAST(st, expr1, expr2, binOp, binOp.returnType)
        }
    }
}
