package ic.doc.group15.codegen.assembly.instruction

import ic.doc.group15.codegen.assembly.Instruction
import ic.doc.group15.codegen.assembly.operand.Operand
import ic.doc.group15.codegen.assembly.operand.Register

/**
 * Arithmetic operations present in ARM1176JZF-S, partly implemented.
 */
abstract class ArithmeticInstruction protected constructor(
    instr: String,
    vararg operands: Operand
) : Instruction(instr, *operands) {

    constructor(
        instr: String,
        operands: List<Operand>
    ) : this(instr, *operands.toTypedArray())
}

abstract class FlexibleIntArithmetic protected constructor(
    instr: String,
    val dest: Register,
    val base: Register,
    val op: Operand
) : ArithmeticInstruction(instr, dest, base, op)

/**
 * ADD is an add-without-carry instruction that adds the values in
 * the base register and operand, and stores the result in the
 * destination register.
 *
 * @param dest The destination register
 * @param base The base register/register holding the first operand
 * @param op A flexible second operand
 */
class Add(
    dest: Register,
    base: Register,
    op: Operand
) : FlexibleIntArithmetic("add", dest, base, op)

/**
 * ADDS is an add-without-carry instruction that adds the values in
 * the base register and operand, and stores the result in the
 * destination register. Since the S suffix is specified, the condition flags
 * N,Z,C,V are updated on the result of the operation.
 *
 * @param dest The destination register
 * @param base The base register/register holding the first operand
 * @param op A flexible second operand
 */
class AddCond(
    dest: Register,
    base: Register,
    op: Operand
) : FlexibleIntArithmetic("adds", dest, base, op)

/**
 * SUB is a subtract-without-carry instruction that subtracts the value of
 * operand from the value in the base register, and stores the result in
 * the destination register.
 *
 * @param dest The destination register
 * @param base The source register/register holding the first operand
 * @param op A flexible second operand
 */
class Sub(
    dest: Register,
    base: Register,
    op: Operand
) : FlexibleIntArithmetic("sub", dest, base, op)

class RSB(
    dest: Register,
    base: Register,
    op: Operand
): FlexibleIntArithmetic("rsb", dest, base, op)

/**
 * SUBS is a subtract-without-carry instruction that subtracts the value of
 * operand from the value in the base register, and stores the result in
 * the destination register. Since the S suffix is specified, the condition flags
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
class SubCond(
    dest: Register,
    base: Register,
    op: Operand
) : FlexibleIntArithmetic("subs", dest, base, op)

abstract class RegisterIntArithmetic protected constructor(
    instr: String,
    val dest: Register,
    val reg_n: Register,
    val reg_m: Register,
) : ArithmeticInstruction(instr, dest, reg_n, reg_m)

/**
 * MUL is a multiply instruction that multiplies the value of
 * register n with the value of register m, and stores the least
 * significant 32 bits of the result in the destination register.
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
    dest: Register,
    reg_n: Register,
    reg_m: Register
) : RegisterIntArithmetic("mul", dest, reg_n, reg_m)

/**
 * MULS is a multiply instruction that multiplies the value of
 * register n with the value of register m, and stores the least
 * significant 32 bits of the result in the destination register.
 * Since the S suffix is specified, the instruction updates the N and Z flags
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
class MultCond(
    dest: Register,
    reg_n: Register,
    reg_m: Register
) : RegisterIntArithmetic("muls", dest, reg_n, reg_m)

abstract class LongArithmetic protected constructor(
    instr: String,
    val dest_lo: Register,
    val dest_hi: Register,
    val reg_n: Register,
    val reg_m: Register
) : ArithmeticInstruction(instr, dest_lo, dest_hi, reg_n, reg_m)

/**
 * SMULL is a signed-long multiply instruction that multiplies the value of
 * register n with the value of register m by treating both values as two's
 * complement signed integers, and stores the least significant 32 bits of the
 * result in low destination register, and the most significant 32 bits of the
 * result in high destination register.
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
    dest_lo: Register,
    dest_hi: Register,
    reg_n: Register,
    reg_m: Register
) : LongArithmetic("smull", dest_lo, dest_hi, reg_n, reg_m)

/**
 * SMULLS is a signed-long multiply instruction that multiplies the value of
 * register n with the value of register m by treating both values as two's
 * complement signed integers, and stores the least significant 32 bits of the
 * result in low destination register, and the most significant 32 bits of the
 * result in high destination register. Since the S suffix is set, the
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
class LongMultCond(
    dest_lo: Register,
    dest_hi: Register,
    reg_n: Register,
    reg_m: Register
) : LongArithmetic("smulls", dest_lo, dest_hi, reg_n, reg_m)
