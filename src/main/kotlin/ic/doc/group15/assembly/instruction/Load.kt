package ic.doc.group15.assembly.instruction

import ic.doc.group15.assembly.* // ktlint-disable no-wildcard-imports
import ic.doc.group15.assembly.operand.AddressOperand
import ic.doc.group15.assembly.operand.Register
import ic.doc.group15.util.BYTE
import ic.doc.group15.assembly.operand.*

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
 * Returns an instance of LoadInstruction specific to the size of the data being loaded.
 */
fun Load(
    size: Int,
    dest: Register,
    addr: AddressOperand
): LoadInstruction {
    return Load(size, null, dest, addr)
}

fun Load(
    size: Int,
    conditionCode: ConditionCode?,
    dest: Register,
    addr: AddressOperand
): LoadInstruction {
    return if (size == BYTE) {
        LoadByte(conditionCode, dest, addr)
    } else {
        LoadWord(conditionCode, dest, addr)
    }
}

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
) : LoadInstruction("LDR", conditionCode, dest, addr) {

    constructor(dest: Register, addr: AddressOperand) : this(null, dest, addr)

    override fun usesSet(): Set<Register> {
        return when (addr) {
            is ImmediateOffset -> {
                setOf(addr.base)
            }
            is ZeroOffset -> {
                setOf(addr.base)
            }
            is RegisterOffset -> {
                setOf(addr.base, addr.offsetReg)
            }
            else -> emptySet()
        }
    }
    override fun definesSet(): Set<Register> = setOf(dest)
}

/**
 * LDRB is a load register instruction that loads the particular unsigned value
 * of byte size (1 byte), zero-extended to 32 bits, at a particular address into
 * the destination register. This instruction is mainly used to load a character.
 *
 * @param dest The destination register
 * @param addr The source address
 */
class LoadByte(
    conditionCode: ConditionCode?,
    dest: Register,
    addr: AddressOperand
) : LoadInstruction("LDRB", conditionCode, dest, addr) {

    constructor(dest: Register, addr: AddressOperand) : this(null, dest, addr)

    override fun usesSet(): Set<Register> {
        return when (addr) {
            is ImmediateOffset -> {
                setOf(addr.base)
            }
            is ZeroOffset -> {
                setOf(addr.base)
            }
            is RegisterOffset -> {
                setOf(addr.base, addr.offsetReg)
            }
            else -> emptySet()
        }
    }
    override fun definesSet(): Set<Register> = setOf(dest)
}
