package ic.doc.group15.codegen.assembly.instruction

import ic.doc.group15.codegen.assembly.Instruction
import ic.doc.group15.codegen.assembly.operand.AddressOperand
import ic.doc.group15.codegen.assembly.operand.Register

abstract class StoreInstruction protected constructor(
    instr: String,
    conditionCode: ConditionCode?,
    val src: Register,
    val addr: AddressOperand
) : Instruction(instr, conditionCode, src, addr)

/**
 * STR is a store register instruction that stores the particular value
 * of word size (4 bytes) at the register into memory at the specified
 * address.
 *
 * @param src The source register
 * @param addr The destination address
 */
class StoreWord(
    conditionCode: ConditionCode?,
    src: Register,
    addr: AddressOperand
) : StoreInstruction("STR", conditionCode, src, addr) {

    constructor(src: Register, addr: AddressOperand) : this(null, src, addr)
}

/**
 * STRB is a store register instruction that stores the particular value
 * of byte size (1 byte) at the register into memory at the specified
 * address.
 *
 * @param src The source register
 * @param addr The destination address
 */
class StoreByte(
    conditionCode: ConditionCode?,
    src: Register,
    addr: AddressOperand
) : StoreInstruction("STRB", conditionCode, src, addr) {

    constructor(src: Register, addr: AddressOperand) : this(null, src, addr)
}
