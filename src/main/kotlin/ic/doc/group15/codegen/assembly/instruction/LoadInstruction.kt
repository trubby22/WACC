package ic.doc.group15.codegen.assembly.instruction

import ic.doc.group15.codegen.assembly.*
import ic.doc.group15.codegen.assembly.operand.AddressOperand
import ic.doc.group15.codegen.assembly.operand.Register

/**
 * Load operations present in ARM1176JZF-S, partly implemented.
 */
abstract class LoadInstruction protected constructor(
    instr: String,
    val dest: Register,
    val addr: AddressOperand
) : Instruction(instr, dest, addr)

/**
 * LDR is a load register instruction that loads the particular value
 * of word size (4 bytes) at a particular address into the destination register.
 *
 * @param dest The destination register
 * @param addr The source address
 */
class LoadWord(dest: Register, addr: AddressOperand) : LoadInstruction("ldr", dest, addr)

/**
 * LDRB is a load register instruction that loads the particular value
 * of byte size (1 byte) at a particular address into the destination register.
 * This instruction is mainly used to load a character.
 *
 * @param dest The destination register
 * @param addr The source address
 */
class LoadByte(dest: Register, addr: AddressOperand) : LoadInstruction("ldrb", dest, addr)

///**
// * LDRSB is a load register instruction that loads the particular value
// * of signed byte size (1 byte) at a particular address into the destination register.
// * This instruction is mainly used to load a boolean.
// *
// * @param dest The destination register
// * @param addr The source address
// */
//class LoadSignedByte(dest: Register, addr: AddressMode3) : LoadInstruction("ldrsb", dest, addr)
