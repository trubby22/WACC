package ic.doc.group15.type

import ic.doc.group15.SymbolTable

const val INT_MAX = Int.MAX_VALUE
const val INT_MIN = Int.MIN_VALUE

interface Identifier

interface Type : Identifier {

    fun compatible(type: Type): Boolean

    companion object {
        val ANY: Type = AnyType()

        private class AnyType : Type {
            override fun compatible(type: Type): Boolean = true

            override fun toString(): String {
                return "any"
            }
        }
    }
}

interface ReturnableType : Type

interface HeapAllocatedType : Type

// we assign the stackPos to be Int.MIN_VALUE until stackPos is actually assigned
open class Variable(
    val type: Type,
) : Identifier {
    companion object {
        val ANY_VAR = Variable(Type.ANY)
    }
}

class Param(type: Type) : Variable(type)

enum class BasicType : ReturnableType {
    IntType {
        override fun compatible(type: Type): Boolean {
            return type == IntType
        }

        override fun toString(): String {
            return "int"
        }
    },
    BoolType {
        override fun compatible(type: Type): Boolean {
            return type == BoolType
        }

        override fun toString(): String {
            return "bool"
        }
    },
    CharType {
        override fun compatible(type: Type): Boolean {
            return type == CharType
        }

        override fun toString(): String {
            return "char"
        }
    },
    StringType {
        override fun compatible(type: Type): Boolean {
            return type == StringType
        }

        override fun toString(): String {
            return "string"
        }
    }
}

class ArrayType(elementType: Type, dimension: Int) : ReturnableType, HeapAllocatedType {

    val elementType: Type
    val dimension: Int

    companion object {
        val ANY_ARRAY = ArrayType(Type.ANY, 1)
    }

    init {
        val type: Type
        val dim: Int
        if (elementType is ArrayType) {
            assert(elementType.elementType !is ArrayType)
            type = elementType.elementType
            dim = dimension + 1
        } else {
            type = elementType
            dim = dimension
        }
        this.elementType = type
        this.dimension = dim
    }

    override fun compatible(type: Type): Boolean {
        if (type !is ArrayType) {
            return false
        }
        if (elementType == Type.ANY || type.elementType == Type.ANY) {
            return true
        }
        if (dimension != type.dimension) {
            return false
        }
        assert(elementType !is ArrayType)
        assert(type.elementType !is ArrayType)
        return elementType.compatible(type.elementType)
    }

    override fun toString(): String {
        return elementType.toString() + "[]".repeat(dimension)
    }
}

class PairType(
    fstType: Type = Type.ANY,
    sndType: Type = Type.ANY
) : ReturnableType, HeapAllocatedType {

    val fstType: Type = if (fstType is PairType) PairType() else fstType
    val sndType: Type = if (sndType is PairType) PairType() else sndType

    companion object {
        val ANY_PAIR = PairType()
    }

    override fun compatible(type: Type): Boolean {
        if (type !is PairType) {
            return false
        }
        return (fstType.compatible(type.fstType) || type.fstType == Type.ANY) &&
            (sndType.compatible(type.sndType) || type.sndType == Type.ANY)
    }

    override fun toString(): String {
        return "pair($fstType, $sndType)"
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
