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

data class Variable(val type: Type) : Identifier

data class Param(val type: Type) : Identifier

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

class ArrayType(val elementType: Type, val size: Int) : ReturnableType, HeapAllocatedType {
    override fun compatible(type: Type): Boolean {
        if (type !is ArrayType) {
            return false
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
        return fstType.compatible(type.fstType) && sndType.compatible(type.sndType)
    }
}

//class FunctionType(
//    val returnType: Type,
//    val formals: List<Param>,
//    val symbolTable: SymbolTable
//) : Type {
//    override fun compatible(type: Type): Boolean {
//        if (!type.compatible(returnType)) {
//            return false
//        }
//        for (i in formals.indices) {
//            if (!formals[i].type.compatible(type.formals[i].type)) {
//                return false
//            }
//        }
//        return true
//    }
//}
