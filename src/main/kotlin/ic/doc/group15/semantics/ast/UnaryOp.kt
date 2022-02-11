package ic.doc.group15.semantics.ast

import ic.doc.group15.semantics.*
import ic.doc.group15.semantics.BasicType.*
import kotlin.reflect.KClass

enum class UnaryOp(
    private val expectedTypeClass: KClass<out ReturnableType>,
    val returnType: ReturnableType
) {
    BANG(BoolType::class, BoolType),
    MINUS(IntType::class, IntType),
    LEN(ArrayType::class, IntType),
    ORD(CharType::class, IntType),
    CHR(IntType::class, CharType);

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
