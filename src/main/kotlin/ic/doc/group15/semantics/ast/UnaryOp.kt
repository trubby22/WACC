package ic.doc.group15.semantics.ast

import ic.doc.group15.semantics.*
import kotlin.reflect.KClass

enum class UnaryOp(
    private val expectedTypeClass: KClass<out ReturnableType>
) {
    BANG(BasicType.BoolType::class),
    MINUS(BasicType.IntType::class),
    LEN(ArrayType::class),
    ORD(BasicType.CharType::class),
    CHR(BasicType.IntType::class);

    fun generateNode(
        st: SymbolTable,
        expr: ExpressionAST
    ): UnaryOpExprAST {
        if (expectedTypeClass != expr.type::class) {
            throw TypeError(
                "operands of binary operation expressions must have the same type"
            )
        }
        return UnaryOpExprAST(st, expr, this)
    }
}
