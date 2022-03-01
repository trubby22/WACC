package ic.doc.group15.instructions

abstract class LogicalInstruction: Instruction()

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
class AND(val dest: Register, val base: Register, val op: Operand2): LogicalInstruction() {
  override fun translate(): String {
    return "and $dest, $base, $op"
  }
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
class ANDS(val dest: Register, val base: Register, val op: Operand2): LogicalInstruction() {
  override fun translate(): String {
    return "ands $dest, $base, $op"
  }
}