package ic.doc.group15.ast

import ic.doc.group15.type.*
import ic.doc.group15.type.BasicType.*

enum class UnaryOp(
    val str: String,
    val expectedType: Type,
    val returnType: ReturnableType
) {
    BANG("!", BoolType, BoolType),
    MINUS("-", IntType, IntType),
    LEN("len", ArrayType.ANY_ARRAY, IntType),
    ORD("ord", CharType, IntType),
    CHR("chr", IntType, CharType);
}
