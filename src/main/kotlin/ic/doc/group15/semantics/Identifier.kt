package ic.doc.group15.semantics

const val INT_MAX = Int.MAX_VALUE
const val INT_MIN = Int.MIN_VALUE

interface Identifier

interface Type : Identifier

interface ReturnableType : Type

data class Variable(val type: Type) : Identifier

data class Param(val type: Type) : Identifier

enum class BasicType : ReturnableType {
    IntType,
    BoolType,
    CharType,
    StringType
}

data class ArrayType(val elementType: Type, val size: Int) : ReturnableType

data class PairType(val leftType: Type?, val rightType: Type?) : ReturnableType

data class FunctionType(
    val returnType: Type?,
    val formals: List<Param>,
    val symbolTable: SymbolTable
) : Type
