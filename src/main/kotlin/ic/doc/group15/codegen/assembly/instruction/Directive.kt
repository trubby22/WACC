package ic.doc.group15.codegen.assembly.instruction

import ic.doc.group15.codegen.assembly.Instruction

class Directive private constructor(labelName: String) : Instruction(labelName) {
    companion object {
        val LTORG = Directive(".ltorg")
    }
}