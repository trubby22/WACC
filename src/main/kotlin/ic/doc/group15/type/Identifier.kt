package ic.doc.group15.type

sealed interface Identifier

// we assign the stackPos to be Int.MIN_VALUE until stackPos is actually assigned
open class Variable(
    val type: VariableType,
) : Identifier {
    companion object {
        val ANY_VAR = Variable(Type.ANY)
    }

    var stackPosition: Int = 0
}

class Param(type: VariableType) : Variable(type)
