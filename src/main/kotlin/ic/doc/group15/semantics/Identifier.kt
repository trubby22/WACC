package ic.doc.group15.semantics

const val INT_MAX = Int.MAX_VALUE
const val INT_MIN = Int.MIN_VALUE

interface Identifier

interface Type : Identifier {
    fun compatible(type: Type): Boolean

    companion object {
        val ANY: Type = AnyType()

        private class AnyType : Type {
            override fun compatible(type: Type): Boolean = true
        }
    }
}

interface ReturnableType : Type

interface HeapAllocatedType : Type

// we assign the stackPos to be Int.MIN_VALUE until stackPos is actually assigned
open class Variable(val type: Type, var stackPos : Int = Int.MIN_VALUE) : Identifier

class Param(type: Type) : Variable(type)

enum class BasicType : ReturnableType {
    IntType {
        override fun compatible(type: Type): Boolean {
            return type == IntType
        }
    },
    BoolType {
        override fun compatible(type: Type): Boolean {
            return type == BoolType
        }
    },
    CharType {
        override fun compatible(type: Type): Boolean {
            return type == CharType
        }
    },
    StringType {
        override fun compatible(type: Type): Boolean {
            return type == StringType
        }
    }
}

class ArrayType(val elementType: Type) : ReturnableType, HeapAllocatedType {
    override fun compatible(type: Type): Boolean {
        if (type !is ArrayType) {
            return false
        }
        if (elementType == Type.ANY || type.elementType == Type.ANY) {
            return true
        }
        return elementType.compatible(type.elementType)
    }
}

class PairType(
    fstType: Type = Type.ANY,
    sndType: Type = Type.ANY
) : ReturnableType, HeapAllocatedType {

    val fstType: Type
    val sndType: Type

    init {
        this.fstType = if (fstType is PairType) PairType() else fstType
        this.sndType = if (sndType is PairType) PairType() else sndType
    }

    override fun compatible(type: Type): Boolean {
        if (type !is PairType) {
            return false
        }
        return (fstType.compatible(type.fstType) || type.fstType == Type.ANY) &&
            (sndType.compatible(type.sndType) || type.sndType == Type.ANY)
    }
}

class FunctionType(
    val returnType: Type,
    val formals: List<Param>,
    val symbolTable: SymbolTable
) : Type {
    override fun compatible(type: Type): Boolean {
        if (type !is FunctionType || type.returnType != returnType) {
            return false
        }
        return true
    }
}
