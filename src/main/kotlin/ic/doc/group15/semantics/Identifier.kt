package ic.doc.group15.semantics

/**
 * TODO: keep reference to AST node
 */
open class Identifier

open class Type() : Identifier()

data class Variable(val type: Type) : Identifier()

data class Param(val type: Type) : Identifier()

open class ReturnableType() : Type()

data class IntType(val min: Int, val max: Int) : ReturnableType()

data class FloatType(val min: Float, val max: Float) : ReturnableType()

data class CharType(val min: Int, val max: Int) : ReturnableType()

class StringType : ReturnableType()

data class BoolType(val falseVal: Int, val trueVal: Int) : ReturnableType()

data class ArrayType(val elementType: Type, val size: Int) : ReturnableType()

data class PairType(val leftType: Type, val rightType: Type) : ReturnableType()

data class FunctionType(
    val returnType: Type?,
    val formals: List<Param>,
    val symbolTable: SymbolTable
) : Type()
