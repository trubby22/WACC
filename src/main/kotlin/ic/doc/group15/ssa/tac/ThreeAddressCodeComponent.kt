package ic.doc.group15.ssa.tac

import ic.doc.group15.type.BasicType.Companion.BoolType
import ic.doc.group15.type.BasicType.Companion.CharType
import ic.doc.group15.type.BasicType.Companion.IntType
import ic.doc.group15.type.BasicType.Companion.StringType
import ic.doc.group15.type.ReturnableType
import ic.doc.group15.type.Type

sealed interface TacOperand {
    fun type(): Type
}

interface Imm : TacOperand

data class IntImm(val value: Int) : Imm {
    override fun type(): Type = IntType
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as IntImm

        if (value != other.value) return false

        return true
    }

    override fun hashCode(): Int {
        return value
    }
}

data class CharImm(val value: Char) : Imm {
    override fun type(): Type = CharType
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as CharImm

        if (value != other.value) return false

        return true
    }

    override fun hashCode(): Int {
        return value.hashCode()
    }
}

data class BoolImm(val value: Boolean) : Imm {
    override fun type(): Type = BoolType
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as BoolImm

        if (value != other.value) return false

        return true
    }

    override fun hashCode(): Int {
        return value.hashCode()
    }
}

data class StrImm(val value: String) : Imm {
    override fun type(): Type = StringType
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as StrImm

        if (value != other.value) return false

        return true
    }

    override fun hashCode(): Int {
        return value.hashCode()
    }
}

data class TacVar(
    val id: Int,
    val type: Type
) : TacOperand {
    // Can store WORD sized value by default, but can also be BYTE: check
    // assignment for actual type
    override fun type(): Type = type
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as TacVar

        if (id != other.id) return false
        if (type != other.type) return false

        return true
    }

    override fun hashCode(): Int {
        var result = id
        result = 31 * result + type.hashCode()
        return result
    }
}

interface Func {
    fun type(): Type
}

data class CustomFunc(val name: String, val type: Type) : Func {
    override fun type(): Type = type
}

enum class Functions(private val type: ReturnableType = ReturnableType.VOID) : Func {
    BANG(BoolType),
    LEN(IntType),
    ORD(IntType),
    CHR(CharType),
    LSL(IntType),
//    LSR(IntType),
    ASR(IntType),
//    FST(IntType),
//    SND(IntType),
    RETURN(IntType),
    EXIT(IntType),
    READ,
    FREE,
    PRINT,
    PRINTLN
    ;

    override fun type(): Type = type
}
