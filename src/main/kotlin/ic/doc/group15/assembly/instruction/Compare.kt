package ic.doc.group15.assembly.instruction

import ic.doc.group15.assembly.Instruction
import ic.doc.group15.assembly.operand.Operand
import ic.doc.group15.assembly.operand.Register

/**
 * Comparison operations present in ARM1176JZF-S, partly implemented.
 */
abstract class CompareInstruction protected constructor(
    instr: String,
    conditionCode: ConditionCode?,
    val base: Register,
    val op: Operand
) : Instruction(instr, conditionCode, base, op)

/**
 * CMP is a compare instruction that compares the value in the register with the
 * operand by subtracting the value of operand from the value in the register,
 * then update the condition flags N,Z,C,V on the result, but do not place the
 * result in any register. It is similar to a call to the SUBS instruction except
 * that the result is discarded.
 *
 * @param base The base register/register holding the first operand
 * @param op A flexible second operand
 */
class Compare(
    conditionCode: ConditionCode?,
    base: Register,
    op: Operand
) : CompareInstruction("CMP", conditionCode, base, op) {

    constructor(base: Register, op: Operand) : this(null, base, op)
}
