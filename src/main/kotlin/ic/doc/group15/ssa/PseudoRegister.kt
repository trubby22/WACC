package ic.doc.group15.ssa

import ic.doc.group15.assembly.operand.Register

class PseudoRegister(val id: Int) : Register {
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as PseudoRegister

        if (id != other.id) return false

        return true
    }

    override fun hashCode(): Int {
        return id
    }

}
