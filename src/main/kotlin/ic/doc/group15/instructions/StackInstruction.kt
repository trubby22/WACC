package ic.doc.group15.instructions

/**
 * Stack operations present in ARM1176JZF-S, partly implemented.
 */
abstract class StackInstruction: Instruction()

/**
 * PUSH is a stack-related instruction that pushes registers onto a full
 * descending stack. It is synonymous with "STMDB sp!, regList" and is the
 * preferred mnemonic. Registers are stored on the stack in numerical order,
 * with the lowest numbered register at the lowest address.
 *
 * @param regList A non-empty list of registers
 */
class PUSH(val regList: List<Register>): StackInstruction() {
  override fun translate(): String {
    return "push ${regList.sorted().joinToString(separator = ",", prefix = "{", postfix = "}")}"
  }
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
 * @param regList A non-empty list of registers
 */
class POP(val regList: List<Register>): StackInstruction() {
  override fun translate(): String {
    return "pop ${regList.sorted().joinToString(separator = ",", prefix = "{", postfix = "}")}"
  }
}