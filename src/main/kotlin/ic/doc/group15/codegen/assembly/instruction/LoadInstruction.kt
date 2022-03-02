package ic.doc.group15.codegen.assembly.instruction

import ic.doc.group15.codegen.assembly.* // ktlint-disable no-wildcard-imports
import ic.doc.group15.codegen.assembly.operand.AddressOperand
import ic.doc.group15.codegen.assembly.operand.Register

/**
 * Load operations present in ARM1176JZF-S, partly implemented.
 */
abstract class LoadInstruction protected constructor(
    instr: String,
    conditionCode: ConditionCode?,
    val dest: Register,
    val addr: AddressOperand
) : Instruction(instr, conditionCode, dest, addr)

/**
 * LDR is a load register instruction that loads the particular value
 * of word size (4 bytes) at a particular address into the destination register.
 *
 * @param dest The destination register
 * @param addr The source address
 */
class LoadWord(
    conditionCode: ConditionCode?,
    dest: Register,
    addr: AddressOperand
) : LoadInstruction("ldr", conditionCode, dest, addr) {

    constructor(dest: Register, addr: AddressOperand) : this(null, dest, addr)
}

/**
 * LDRB is a load register instruction that loads the particular value
 * of byte size (1 byte) at a particular address into the destination register.
 * This instruction is mainly used to load a character.
 *
 * @param dest The destination register
 * @param addr The source address
 */
class LoadByte(
    conditionCode: ConditionCode?,
    dest: Register,
    addr: AddressOperand
) : LoadInstruction("ldrb", conditionCode, dest, addr) {

    constructor(dest: Register, addr: AddressOperand) : this(null, dest, addr)
}

///**
// * LDRSB is a load register instruction that loads the particular value
// * of signed byte size (1 byte) at a particular address into the destination register.
// * This instruction is mainly used to load a boolean.
// *
// * @param dest The destination register
// * @param addr The source address
// */
//class LoadSignedByte(dest: Register, addr: AddressMode3) : LoadInstruction("ldrsb", dest, addr)
