package ic.doc.group15.codegen.assembly.instruction

import ic.doc.group15.codegen.assembly.Instruction
import ic.doc.group15.codegen.assembly.operand.Operand
import ic.doc.group15.codegen.assembly.operand.Register

/**
 * Arithmetic operations present in ARM1176JZF-S, partly implemented.
 */
abstract class UpdateFlagsInstruction protected constructor(
    instr: String,
    conditionCode: ConditionCode?,
    val updateFlags: Boolean = false,
    vararg operands: Operand
) : Instruction(instr, conditionCode, *operands) {

    constructor(
        instr: String,
        conditionCode: ConditionCode?,
        updateFlags: Boolean = false,
        operands: List<Operand>
    ) : this(instr, conditionCode, updateFlags, *operands.toTypedArray())

    override fun toString(): String {
        return "$instr${conditionCode ?: ""}${if (updateFlags) "S" else ""} " + params
            .joinToString(separator = ", ")
    }
}

abstract class FlexibleIntArithmetic protected constructor(
    instr: String,
    conditionCode: ConditionCode?,
    updateFlags: Boolean = false,
    val dest: Register,
    val base: Register,
    val op: Operand
) : UpdateFlagsInstruction(instr, conditionCode, updateFlags, dest, base, op)

/**
 * ADD is an add-without-carry instruction that adds the values in
 * the base register and operand, and stores the result in the
 * destination register. If the S suffix is specified, the condition flags
 * N,Z,C,V are updated on the result of the operation.
 *
 * @param dest The destination register
 * @param base The base register/register holding the first operand
 * @param op A flexible second operand
 */
class Add(
    conditionCode: ConditionCode?,
    updateFlags: Boolean,
    dest: Register,
    base: Register,
    op: Operand
) : FlexibleIntArithmetic("ADD", conditionCode, updateFlags, dest, base, op) {

    constructor(conditionCode: ConditionCode, dest: Register, base: Register, op: Operand) : this(
        conditionCode,
        false,
        dest,
        base,
        op
    )

    constructor(updateFlags: Boolean, dest: Register, base: Register, op: Operand) : this(
        null,
        updateFlags,
        dest,
        base,
        op
    )

    constructor(dest: Register, base: Register, op: Operand) : this(null, false, dest, base, op)
}

/**
 * SUB is a subtract-without-carry instruction that subtracts the value of
 * operand from the value in the base register, and stores the result in
 * the destination register. If the S suffix is specified, the condition flags
 * N,Z,C,V are updated on the result of the operation.
 *
 * Note: "SUBS pc, lr #imm" is a special case of the instruction - it performs
 * exception return without popping anything from the stack. In other words,
 * it subtracts the value from the link register and loads the PC with the
 * result, then copies the SPSR to the CPSR. "SUBS pc, lr #imm" can be used
 * to return from an exception if there is no return state on the stack. The
 * value of #imm depends on the exception to return from.
 *
 * @param dest The destination register
 * @param base The source register/register holding the first operand
 * @param op A flexible second operand
 */
class Sub(
    conditionCode: ConditionCode?,
    updateFlags: Boolean,
    dest: Register,
    base: Register,
    op: Operand
) : FlexibleIntArithmetic("SUB", conditionCode, updateFlags, dest, base, op) {

    constructor(conditionCode: ConditionCode, dest: Register, base: Register, op: Operand) : this(
        conditionCode,
        false,
        dest,
        base,
        op
    )

    constructor(updateFlags: Boolean, dest: Register, base: Register, op: Operand) : this(
        null,
        updateFlags,
        dest,
        base,
        op
    )

    constructor(dest: Register, base: Register, op: Operand) : this(null, false, dest, base, op)
}

class ReverseSub(
    conditionCode: ConditionCode?,
    updateFlags: Boolean,
    dest: Register,
    base: Register,
    op: Operand
) : FlexibleIntArithmetic("RSB", conditionCode, updateFlags, dest, base, op) {

    constructor(conditionCode: ConditionCode, dest: Register, base: Register, op: Operand) : this(
        conditionCode,
        false,
        dest,
        base,
        op
    )

    constructor(updateFlags: Boolean, dest: Register, base: Register, op: Operand) : this(
        null,
        updateFlags,
        dest,
        base,
        op
    )

    constructor(dest: Register, base: Register, op: Operand) : this(null, false, dest, base, op)
}

abstract class RegisterIntArithmetic protected constructor(
    instr: String,
    conditionCode: ConditionCode?,
    updateFlags: Boolean = false,
    val dest: Register,
    val reg_n: Register,
    val reg_m: Register,
) : UpdateFlagsInstruction(instr, conditionCode, updateFlags, dest, reg_n, reg_m)

/**
 * MUL is a multiply instruction that multiplies the value of
 * register n with the value of register m, and stores the least
 * significant 32 bits of the result in the destination register.
 * If the S suffix is specified, the instruction updates the N and Z flags
 * according to the result.
 *
 * Note: Register n must be different from the destination register before
 * ARMv6; PC cannot be used for any register; and SP can be used but this is
 * deprecated in ARMv6T2 and above.
 *
 * @param dest The destination register
 * @param reg_n The source register/register holding the first operand
 * @param reg_m The source register/register holding the second operand
 */
class Mult(
    conditionCode: ConditionCode?,
    updateFlags: Boolean,
    dest: Register,
    reg_n: Register,
    reg_m: Register
) : RegisterIntArithmetic("MUL", conditionCode, updateFlags, dest, reg_n, reg_m) {

    constructor(conditionCode: ConditionCode, dest: Register, reg_n: Register, reg_m: Register) : this(
        conditionCode,
        false,
        dest,
        reg_n,
        reg_m
    )

    constructor(updateFlags: Boolean, dest: Register, reg_n: Register, reg_m: Register) : this(
        null,
        updateFlags,
        dest,
        reg_n,
        reg_m
    )

    constructor(dest: Register, reg_n: Register, reg_m: Register) : this(null, false, dest, reg_n, reg_m)
}

abstract class LongArithmetic protected constructor(
    instr: String,
    conditionCode: ConditionCode?,
    updateFlags: Boolean = false,
    val dest_lo: Register,
    val dest_hi: Register,
    val reg_n: Register,
    val reg_m: Register
) : UpdateFlagsInstruction(instr, conditionCode, updateFlags, dest_lo, dest_hi, reg_n, reg_m)

/**
 * SMULL is a signed-long multiply instruction that multiplies the value of
 * register n with the value of register m by treating both values as two's
 * complement signed integers, and stores the least significant 32 bits of the
 * result in low destination register, and the most significant 32 bits of the
 * result in high destination register. If the S suffix is set, the
 * instruction updates the N and Z flags according to the result.
 *
 * Note: Low destination register must be different from high destination register;
 * PC cannot be used for any register
 *
 * @param dest_lo The low destination register
 * @param dest_hi The high destination register
 * @param reg_n The source register/register holding the first operand
 * @param reg_m The source register/register holding the second operand
 */
class LongMult(
    conditionCode: ConditionCode?,
    updateFlags: Boolean,
    dest_lo: Register,
    dest_hi: Register,
    reg_n: Register,
    reg_m: Register
) : LongArithmetic("SMULL", conditionCode, updateFlags, dest_lo, dest_hi, reg_n, reg_m) {

    constructor(
        conditionCode: ConditionCode,
        dest_lo: Register,
        dest_hi: Register,
        reg_n: Register,
        reg_m: Register
    ) : this(conditionCode, false, dest_lo, dest_hi, reg_n, reg_m)

    constructor(updateFlags: Boolean, dest_lo: Register, dest_hi: Register, reg_n: Register, reg_m: Register) : this(
        null,
        updateFlags,
        dest_lo,
        dest_hi,
        reg_n,
        reg_m
    )

    constructor(dest_lo: Register, dest_hi: Register, reg_n: Register, reg_m: Register) :
            this(null, false, dest_lo, dest_hi, reg_n, reg_m)
}