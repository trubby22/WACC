package ic.doc.group15.ast

import ic.doc.group15.type.BasicType.*
import ic.doc.group15.type.ReturnableType

private val intOnly = setOf<ReturnableType>(IntType)
private val comparables = setOf<ReturnableType>(IntType, CharType)
private val boolOnly = setOf<ReturnableType>(BoolType)

enum class BinaryOp(
    val str: String,
    val allowedTypes: Set<ReturnableType>? = intOnly,
    val returnType: ReturnableType = BoolType
) {
    MULT("*", returnType = IntType),
    DIV("/", returnType = IntType),
    MOD("%", returnType = IntType),
    PLUS("+", returnType = IntType),
    MINUS("-", returnType = IntType),
    GT(">", allowedTypes = comparables),
    GTE(">=", allowedTypes = comparables),
    LT("<", allowedTypes = comparables),
    LTE("<=", allowedTypes = comparables),
    EQUALS("=", allowedTypes = null),
    NOT_EQUALS("!=", allowedTypes = null),
    AND("&&", allowedTypes = boolOnly),
    OR("||", allowedTypes = boolOnly);
}
