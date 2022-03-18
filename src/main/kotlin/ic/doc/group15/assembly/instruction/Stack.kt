package ic.doc.group15.assembly.instruction

import ic.doc.group15.assembly.Instruction
import ic.doc.group15.assembly.operand.Register
import ic.doc.group15.assembly.operand.RegisterList

/**
 * Stack operations present in ARM1176JZF-S, partly implemented.
 */
abstract class StackInstruction protected constructor(
    instr: String,
    conditionCode: ConditionCode?,
    val registers: Array<out Register>
) : Instruction(instr, conditionCode, RegisterList(*registers))

/**
 * PUSH is a stack-related instruction that pushes registers onto a full
 * descending stack. It is synonymous with "STMDB sp!, regList" and is the
 * preferred mnemonic. Registers are stored on the stack in numerical order,
 * with the lowest numbered register at the lowest address.
 *
 * @param regs The registers to push
 */
class Push(
    conditionCode: ConditionCode?,
    vararg regs: Register
) : StackInstruction("PUSH", conditionCode, regs) {

    constructor(vararg regs: Register) : this(null, *regs)

    override fun usesSet(): Set<Register> = registers.toSet()
    override fun definesSet(): Set<Register> = emptySet()
}

/**
 * POP is a stack-related instruction that pops registers off a full
 * descending stack. It is synonymous with "LDMIA sp!, regList" and is the
 * preferred mnemonic. Registers are stored on the stack in numerical order,
 * with the lowest numbered register at the lowest address.
 *
 * If PC is in the register list, the instruction causes a branch to the address
 * popped off the stack into the PC. This is usually a return from the subroutine,
 * where the LR was pushed onto the stack at the start of the subroutine.
 * The register list cannot contain SP.
 *
 * @param regs The registers to pop to
 */
class Pop(
    conditionCode: ConditionCode?,
    vararg regs: Register
) : StackInstruction("POP", conditionCode, regs) {

    constructor(vararg regs: Register) : this(null, *regs)

    override fun usesSet(): Set<Register> = emptySet()
    override fun definesSet(): Set<Register> = registers.toSet()
}
