package ic.doc.group15.ssa

import ic.doc.group15.type.BasicType
import ic.doc.group15.type.FunctionType
import ic.doc.group15.type.Type

sealed interface Operand {
    fun type(): Type
}

data class StrImm(val value: String): Operand {
    override fun type(): Type = BasicType.StringType
}
data class IntImm(val value: Int): Operand {
    override fun type(): Type = BasicType.IntType
}
data class BoolImm(val value: Boolean): Operand {
    override fun type(): Type = BasicType.BoolType
}
data class Register(val id: Int): Operand {
    // Can store WORD sized value by default, but can also be BYTE: check
    // assignment for actual type
    override fun type(): Type = BasicType.IntType

    override fun toString(): String {
        return "%$id"
    }
}
data class Label(val block: String): Operand {
    override fun type(): Type = BasicType.IntType
}

interface Func {
    fun returnType(): Type
}

// TODO: remove reference of symbol table from returnType - they will not be used later
// Can be done by creating new class without storing symbol table
data class CustomFunc(val name: String, val returnType: FunctionType): Func {
    override fun returnType(): Type = returnType
}

enum class Functions: Func {
    FST {
        override fun returnType(): Type = BasicType.IntType
    },
    SND {
        override fun returnType(): Type = BasicType.IntType
    },
    NEWPAIR {
        override fun returnType(): Type = BasicType.IntType
    },
    RETURN {
        override fun returnType(): Type = BasicType.IntType
    },
    EXIT {
        override fun returnType(): Type = BasicType.IntType
    },
    READ {
        override fun returnType(): Type = BasicType.VoidType
    },
    FREE {
        override fun returnType(): Type = BasicType.VoidType
    },
    PRINT {
        override fun returnType(): Type = BasicType.VoidType
    },
    PRINTLN {
        override fun returnType(): Type = BasicType.VoidType
    }
}