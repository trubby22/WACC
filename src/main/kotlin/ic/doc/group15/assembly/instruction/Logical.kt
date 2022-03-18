package ic.doc.group15.assembly.instruction

import ic.doc.group15.assembly.operand.Operand
import ic.doc.group15.assembly.operand.Register

/**
 * Logical operations present in ARM1176JZF-S, partly implemented.
 */
abstract class LogicalInstruction protected constructor(
    instr: String,
    conditionCode: ConditionCode?,
    updateFlags: Boolean,
    val dest: Register,
    val base: Register,
    val op: Operand
) : UpdateFlagsInstruction(instr, conditionCode, updateFlags, dest, base, op)

/**
 * AND is a logical-and instruction that performs bitwise AND operations on
 * the values in the base register and the operand, and stores the result in
 * the destination register. If the S suffix is set, the instruction updates
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
class And(
    conditionCode: ConditionCode?,
    updateFlags: Boolean,
    dest: Register,
    base: Register,
    op: Operand
) : LogicalInstruction("AND", conditionCode, updateFlags, dest, base, op) {

    constructor(conditionCode: ConditionCode, dest: Register, base: Register, op: Operand) : this(conditionCode, false, dest, base, op)
    constructor(updateFlags: Boolean, dest: Register, base: Register, op: Operand) : this(null, updateFlags, dest, base, op)
    constructor(dest: Register, base: Register, op: Operand) : this(null, false, dest, base, op)

    override fun usesSet(): Set<Register> = setOf(base, op).filterIsInstance<Register>().toSet()
    override fun definesSet(): Set<Register> = setOf(dest)
}

class Or(
    conditionCode: ConditionCode?,
    updateFlags: Boolean,
    dest: Register,
    base: Register,
    op: Operand
) : LogicalInstruction("ORR", conditionCode, updateFlags, dest, base, op) {

    constructor(conditionCode: ConditionCode, dest: Register, base: Register, op: Operand) : this(conditionCode, false, dest, base, op)
    constructor(updateFlags: Boolean, dest: Register, base: Register, op: Operand) : this(null, updateFlags, dest, base, op)
    constructor(dest: Register, base: Register, op: Operand) : this(null, false, dest, base, op)

    override fun usesSet(): Set<Register> = setOf(base, op).filterIsInstance<Register>().toSet()
    override fun definesSet(): Set<Register> = setOf(dest)
}

class Xor(
    conditionCode: ConditionCode?,
    updateFlags: Boolean,
    dest: Register,
    base: Register,
    op: Operand
) : LogicalInstruction("EOR", conditionCode, updateFlags, dest, base, op) {

    constructor(conditionCode: ConditionCode, dest: Register, base: Register, op: Operand) : this(conditionCode, false, dest, base, op)
    constructor(updateFlags: Boolean, dest: Register, base: Register, op: Operand) : this(null, updateFlags, dest, base, op)
    constructor(dest: Register, base: Register, op: Operand) : this(null, false, dest, base, op)

    override fun usesSet(): Set<Register> = setOf(base, op).filterIsInstance<Register>().toSet()
    override fun definesSet(): Set<Register> = setOf(dest)
}
