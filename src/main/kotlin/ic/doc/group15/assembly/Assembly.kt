package ic.doc.group15.assembly

import ic.doc.group15.assembly.instruction.ConditionCode
import ic.doc.group15.assembly.operand.Operand

const val ASM_TAB_SIZE = 8
val ASM_TAB = " ".repeat(ASM_TAB_SIZE)

interface Assembly

abstract class Line : Assembly

abstract class Instruction protected constructor(
    val instr: String,
    val conditionCode: ConditionCode? = null,
    protected vararg val params: Operand
) : Line() {
    override fun toString(): String {
        return "$instr${conditionCode ?: ""}${if (params.isNotEmpty()) " " else ""}" +
            params.joinToString(separator = ", ")
    }
}
