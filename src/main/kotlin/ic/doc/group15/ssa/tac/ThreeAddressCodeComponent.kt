package ic.doc.group15.ssa.tac

import ic.doc.group15.type.BasicType
import ic.doc.group15.type.BasicType.*
import ic.doc.group15.type.Type

sealed interface Operand {
    fun type(): Type
}

data class CharImm(val value: Char): Operand {
    override fun type(): Type = CharType
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

data class Var(val id: Int, val name: String, val type: Type) : Operand {
    // Can store WORD sized value by default, but can also be BYTE: check
    // assignment for actual type
    override fun type(): Type = type
}

data class Label(val block: String) : Operand {
    override fun type(): Type = IntType
}

interface Func {
    fun type(): Type
}

data class CustomFunc(val name: String, val type: Type) : Func {
    override fun type(): Type = type
}

abstract class IOFuncs(open val returnType: Type): Func

data class Read(override val returnType: Type): IOFuncs(returnType) {
    override fun type(): Type = returnType
}
data class Print(override val returnType: Type): IOFuncs(returnType) {
    override fun type(): Type = returnType
}
data class Println(override val returnType: Type): IOFuncs(returnType) {
    override fun type(): Type = returnType
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

    override fun type(): Type = type
}
