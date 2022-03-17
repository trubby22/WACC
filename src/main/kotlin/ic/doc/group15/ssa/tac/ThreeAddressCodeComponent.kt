package ic.doc.group15.ssa.tac

import ic.doc.group15.type.BasicType
import ic.doc.group15.type.BasicType.*
import ic.doc.group15.type.Type

sealed interface Operand {
    fun type(): Type
}

interface Imm: Operand

data class CharImm(val value: Char) : Imm {
    override fun type(): Type = CharType
}

data class StrImm(val value: String) : Imm {
    override fun type(): Type = StringType
}

data class IntImm(val value: Int) : Imm {
    override fun type(): Type = IntType
}

data class BoolImm(val value: Boolean) : Imm {
    override fun type(): Type = BoolType
}

data class Var(
    val id: Int,
    val type: Type
) : Operand {
    // Can store WORD sized value by default, but can also be BYTE: check
    // assignment for actual type
    override fun type(): Type = type
}

interface Func {
    fun type(): Type
}

data class CustomFunc(val name: String, val type: Type) : Func {
    override fun type(): Type = type
}

enum class Functions(private val type: BasicType = VoidType) : Func {
    BANG(BoolType),
    LEN(IntType),
    ORD(IntType),
    CHR(CharType),
    LSL(IntType),
    LSR(IntType),
    ASR(IntType),
    FST(IntType),
    SND(IntType),
    RETURN(IntType),
    EXIT(IntType),
    READ,
    FREE,
    PRINT,
    PRINTLN
    ;

    override fun type(): Type = type
}
