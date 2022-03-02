package ic.doc.group15.codegen.assembly.instruction

import ic.doc.group15.codegen.assembly.Instruction
import ic.doc.group15.codegen.assembly.operand.Operand
import ic.doc.group15.codegen.assembly.operand.Register

/**
 * Logical operations present in ARM1176JZF-S, partly implemented.
 */
// abstract class LogicalInstruction(
//    instr: String,
//    vararg operands: Operand<*>
// ) : Instruction(instr, *operands) {
//
//    constructor(
//        instr: String,
//        operands: List<Operand<*>>
//    ) : this(instr, *operands.toTypedArray())
// }
abstract class LogicalInstruction protected constructor(
    instr: String,
    conditionCode: ConditionCode?,
    val updateFlags: Boolean,
    val dest: Register,
    val base: Register,
    val op: Operand
) : Instruction(instr, conditionCode, dest, base, op) {

    override fun toString(): String {
        return "$instr${conditionCode ?: ""}${if (updateFlags) "s" else ""} " + params
            .joinToString(separator = ", ")
    }
}

/**
 * AND is a logical-and instruction that performs bitwise AND operations on
 * the values in the base register and the operand, and stores the result in
 * the destination register.
 *
 * Note: If PC is selected as the base register, the value will be the current
 * instruction address + 8.
 * If PC is selected as the destination register, execution branches to the
 * address corresponding to the result.
 *
 * @param dest The destination register.
 * @param base The base register/register holding the first operand
 * @param op A flexible second operand
 */
class And(
    conditionCode: ConditionCode?,
    dest: Register,
    base: Register,
    op: Operand
) : LogicalInstruction("and", conditionCode, false, dest, base, op) {

    constructor(dest: Register, base: Register, op: Operand) : this(null, dest, base, op)
}

/**
 * AND is a logical-and instruction that performs bitwise AND operations on
 * the values in the base register and the operand, and stores the result in
 * the destination register. Since the S suffix is set, the instruction updates
 * the N and Z flags based on the result and can update the C flag during the
 * calculation of the operand.
 *
 * Note: If PC is selected as the base register, the value will be the current
 * instruction address + 8.
 * If PC is selected as the destination register, execution branches to the
 * address corresponding to the result.
 *
 * @param dest The destination register.
 * @param base The base register/register holding the first operand
 * @param op A flexible second operand
 */
class AndUpdate(
    conditionCode: ConditionCode?,
    dest: Register,
    base: Register,
    op: Operand
) : LogicalInstruction("ands", conditionCode, true, dest, base, op) {

    constructor(dest: Register, base: Register, op: Operand) : this(null, dest, base, op)
}

class Xor(
    conditionCode: ConditionCode?,
    dest: Register,
    base: Register,
    op: Operand
) : LogicalInstruction("eor", conditionCode, false, dest, base, op) {

    constructor(dest: Register, base: Register, op: Operand) : this(null, dest, base, op)
}
