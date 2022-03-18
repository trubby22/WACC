package ic.doc.group15.assembly.instruction

import ic.doc.group15.assembly.Instruction
import ic.doc.group15.assembly.operand.Register

class Directive private constructor(labelName: String) : Instruction(labelName) {
    companion object {
        val LTORG = Directive(".ltorg")
    }

    override fun usesSet(): Set<Register> {
        return emptySet()
    }

    override fun definesSet(): Set<Register> {
        return emptySet()
    }
}
