package ic.doc.group15.codegen.assembly

import ic.doc.group15.codegen.assembly.operand.LabelOperand

class ArmFunction private constructor(labelName: String) : LabelOperand(labelName) {

    companion object {
        val SCANF = ArmFunction("scanf")
    }
}
