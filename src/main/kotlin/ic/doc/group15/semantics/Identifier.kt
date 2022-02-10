package ic.doc.group15.semantics

const val INT_MAX = Int.MAX_VALUE
const val INT_MIN = Int.MIN_VALUE

interface Identifier

interface Type : Identifier {
    fun compatible(type: Type): Boolean
}

interface ReturnableType : Type

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

class ArrayType(val elementType: Type, val size: Int) : ReturnableType {
    override fun compatible(type: Type): Boolean {
        if (type !is ArrayType) {
            return false
        }
        return elementType.compatible(type.elementType)
    }
}

enum class UnaryOp : Identifier {
    BANG,
    MINUS,
    LEN,
    ORD,
    CHR
}

enum class BinaryOp: Identifier {
    MULT,
    DIV,
    MOD,
    PLUS,
    MINUS,
    GT,
    GTE,
    LT,
    LTE,
    EQUALS,
    NOT_EQUALS,
    AND,
    OR
}

class PairType(
    val leftType: Type?,
    val rightType: Type?
) : ReturnableType {

    companion object {
        fun typedPair(leftType: Type, rightType: Type): PairType {
            return PairType(leftType, rightType)
        }

        fun typeErasedPair(): PairType {
            return PairType(null, null)
        }
    }

    override fun compatible(type: Type): Boolean {
        if (type !is PairType) {
            return false
        }
        if (leftType == null || rightType == null) {
            assert(leftType == null && rightType == null)
            return type.leftType == null && type.rightType == null
        }
        return leftType.compatible(type.leftType!!) && rightType.compatible(type.rightType!!)
    }
}

class FunctionType(
    val returnType: Type,
    val formals: List<Param>,
    val symbolTable: SymbolTable
) : Type {
    override fun compatible(type: Type): Boolean {
        if (type !is FunctionType) {
            return false
        }
        for (i in formals.indices) {
            if (!formals[i].type.compatible(type.formals[i].type)) {
                return false
            }
        }
        return true
    }
}
