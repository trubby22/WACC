package ic.doc.group15.ssa.tac

import ic.doc.group15.type.BasicType
import ic.doc.group15.type.BasicType.*
import ic.doc.group15.type.FunctionType
import ic.doc.group15.type.Type

sealed interface Operand {
    fun type(): Type
}

data class StrImm(val value: String) : Operand {
    override fun type(): Type = StringType
}

data class IntImm(val value: Int) : Operand {
    override fun type(): Type = IntType
}

data class BoolImm(val value: Boolean) : Operand {
    override fun type(): Type = BoolType
}

data class Register(val id: Int) : Operand {
    // Can store WORD sized value by default, but can also be BYTE: check
    // assignment for actual type
    override fun type(): Type = IntType
}

data class Label(val block: String) : Operand {
    override fun type(): Type = IntType
}

interface Func {
    fun returnType(): Type
}

data class CustomFunc(val name: String, val func: FunctionType) : Func {
    override fun returnType(): Type = func.returnType
}

enum class Functions(private val type: BasicType = VoidType) : Func {
    FST(IntType),
    SND(IntType),
    NEWPAIR(IntType),
    RETURN(IntType),
    EXIT(IntType),
    READ,
    FREE,
    PRINT,
    PRINTLN
    ;

    override fun returnType(): Type = type
}
