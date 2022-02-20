package ic.doc.group15.semantics.ast

import ic.doc.group15.semantics.*
import ic.doc.group15.semantics.BasicType.*
import kotlin.reflect.KClass

enum class UnaryOp(
    private val str: String,
    private val expectedTypeClass: KClass<out ReturnableType>,
    val returnType: ReturnableType
) {
    BANG("!", BoolType::class, BoolType),
    MINUS("-", IntType::class, IntType),
    LEN("len", ArrayType::class, IntType),
    ORD("ord", CharType::class, IntType),
    CHR("chr", IntType::class, CharType);

    fun generateNode(
        st: SymbolTable,
        expr: ExpressionAST
    ): UnaryOpExprAST {
        if (expectedTypeClass != expr.type::class) {
            throw TypeError(
                "cannot apply $str to ${expr.type} type"
            )
        }
        return UnaryOpExprAST(st, expr, this)
    }
}
