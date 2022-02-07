package ic.doc.group15.semantics

/**
 * TODO: keep reference to AST node
 */
open class Identifier

open class Type : Identifier()

data class Variable(val type: Type) : Identifier()

data class Param(val type: Type) : Identifier()

data class IntType(val min: Int, val max: Int) : Type()

data class FloatType(val min: Float, val max: Float) : Type()

data class CharType(val min: Int, val max: Int) : Type()

class StringType : Type()

data class BoolType(val falseVal: Int, val trueVal: Int) : Type()

data class ArrayType(val elementType: Type, val size: Int) : Type()

data class PairType(val leftType: Type, val rightType: Type) : Type()

data class FunctionType(
    val returnType: Type?,
    val params: List<Param>,
    val symbolTable: SymbolTable
) : Type()