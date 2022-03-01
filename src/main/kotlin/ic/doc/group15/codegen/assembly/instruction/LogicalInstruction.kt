package ic.doc.group15.codegen.assembly.instruction

import ic.doc.group15.codegen.assembly.Instruction
import ic.doc.group15.codegen.assembly.operand.ConditionCode
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
abstract class LogicalInstruction(
    instr: String,
    val dest: Register,
    val base: Register,
    val op: Operand
) : Instruction(instr, dest, base, op)

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
 * @param cond Optional condition code, which only allows execution of instruction if true
 * @param dest The destination register.
 * @param base The base register/register holding the first operand
 * @param op A flexible second operand
 */
class And(
    cond: ConditionCode? = null,
    dest: Register,
    base: Register,
    op: Operand
) : LogicalInstruction("and${cond?.toString() ?: ""}", dest, base, op)

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
 * @param cond Optional condition code, which only allows execution of instruction if true
 * @param dest The destination register.
 * @param base The base register/register holding the first operand
 * @param op A flexible second operand
 */
class AndCond(
    cond: ConditionCode? = null,
    dest: Register,
    base: Register,
    op: Operand
) : LogicalInstruction("and${cond?.toString() ?: ""}s", dest, base, op)

class EOR(
    cond: ConditionCode? = null,
    dest: Register,
    base: Register,
    op: Operand
) : LogicalInstruction("eor${cond?.toString() ?: ""}", dest, base, op)
