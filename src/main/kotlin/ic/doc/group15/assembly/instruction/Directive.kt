package ic.doc.group15.assembly.instruction

import ic.doc.group15.assembly.Instruction

class Directive private constructor(labelName: String) : Instruction(labelName) {
    companion object {
        val LTORG = Directive(".ltorg")
    }
}