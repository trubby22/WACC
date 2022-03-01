package ic.doc.group15.codegen.assembly.instruction

import ic.doc.group15.codegen.assembly.Instruction
import ic.doc.group15.codegen.assembly.operand.AddressOperand
import ic.doc.group15.codegen.assembly.operand.ConditionCode
import ic.doc.group15.codegen.assembly.operand.Register

abstract class StoreInstruction protected constructor(
    instr: String,
    val src: Register,
    val addr: AddressOperand
) : Instruction(instr, src, addr)

/**
 * STR is a store register instruction that stores the particular value
 * of word size (4 bytes) at the register into memory at the specified
 * address.
 *
 * @param cond Optional condition code, which only allows execution of instruction if true
 * @param src The source register
 * @param addr The destination address
 */
class StoreWord(cond: ConditionCode? = null, src: Register, addr: AddressOperand) :
    StoreInstruction("STR${cond?.toString() ?: ""}", src, addr)

/**
 * STRB is a store register instruction that stores the particular value
 * of byte size (1 byte) at the register into memory at the specified
 * address.
 *
 * @param cond Optional condition code, which only allows execution of instruction if true
 * @param src The source register
 * @param addr The destination address
 */
class StoreByte(cond: ConditionCode? = null, src: Register, addr: AddressOperand) :
    StoreInstruction("STR${cond?.toString() ?: ""}B", src, addr)
