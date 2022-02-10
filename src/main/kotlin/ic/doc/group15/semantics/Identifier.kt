package ic.doc.group15.semantics

const val INT_MAX = Int.MAX_VALUE
const val INT_MIN = Int.MIN_VALUE

interface Identifier

interface Type : Identifier

interface ReturnableType : Type

interface GenericArrayType : ReturnableType

data class Variable(val type: Type) : Identifier

data class Param(val type: Type) : Identifier

enum class BasicType : ReturnableType {
    IntType,
    BoolType,
    CharType,
    StringType
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

data class ArrayType(val elementType: Type, val size: Int) : GenericArrayType

data class PairType(val leftType: Type?, val rightType: Type?) : ReturnableType

data class FunctionType(
    val returnType: Type?,
    val formals: List<Param>,
    val symbolTable: SymbolTable
) : Type
