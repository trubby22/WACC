package ic.doc.group15.ast

import ic.doc.group15.type.BasicType.Companion.BoolType
import ic.doc.group15.type.BasicType.Companion.CharType
import ic.doc.group15.type.BasicType.Companion.IntType
import ic.doc.group15.type.VariableType

private val intOnly = setOf<VariableType>(IntType)
private val comparables = setOf<VariableType>(IntType, CharType)
private val boolOnly = setOf<VariableType>(BoolType)

enum class BinaryOp(
    val str: String,
    val allowedTypes: Set<VariableType>? = intOnly,
    val returnType: VariableType = BoolType
) {
    MULT("*", returnType = IntType),
    DIV("/", returnType = IntType),
    MOD("%", returnType = IntType),
    PLUS("+", returnType = IntType),
    MINUS("-", returnType = IntType),
    GT(">", comparables),
    GTE(">=", comparables),
    LT("<", comparables),
    LTE("<=", comparables),
    EQUALS("=", null),
    NOT_EQUALS("!=", null),
    AND("&&", boolOnly),
    OR("||", boolOnly);
}
