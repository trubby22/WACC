package ic.doc.group15.semantics.visitor

import ic.doc.group15.antlr.WaccParser.*
import ic.doc.group15.semantics.*
import ic.doc.group15.semantics.ast.ExpressionAST
import ic.doc.group15.semantics.ast.TypeError
import ic.doc.group15.semantics.ast.UnaryOpExprAST
import kotlin.reflect.KClass

enum class UnaryOp(
    private val opClass: KClass<out Unary_opContext>,
    private val expectedTypeClass: KClass<out ReturnableType>
) {
    BANG(BangUnaryOpContext::class, BasicType.BoolType::class),
    MINUS(MinusUnaryOpContext::class, BasicType.IntType::class),
    LEN(LenUnaryOpContext::class, ArrayType::class),
    ORD(OrdUnaryOpContext::class, BasicType.CharType::class),
    CHR(ChrUnaryOpContext::class, BasicType.IntType::class);

    companion object {
        private val unOpMap = UnaryOp.values().associateBy(UnaryOp::opClass)

        fun generateNode(
            st: SymbolTable,
            expr: ExpressionAST,
            ctx: Unary_opContext
        ): UnaryOpExprAST {
            val unOp = unOpMap[ctx::class]
            assert(unOp != null)
            if (unOp!!.expectedTypeClass != expr.type::class) {
                throw TypeError(
                    "operands of binary operation expressions must have the same type"
                )
            }
            return UnaryOpExprAST(st, expr, unOp)
        }
    }
}
