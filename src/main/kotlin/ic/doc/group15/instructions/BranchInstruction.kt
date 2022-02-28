package ic.doc.group15.instructions

/**
 * Branch operations present in ARM1176JZF-S, partly implemented.
 */
abstract class BranchInstruction: Instruction()

/**
 * B is a branch instruction. Similar to JMP in x86, B causes a branch to label.
 *
 * @param label The label to jump to
 */
class B(val label: Instruction): BranchInstruction() {
  override fun translate(): String {
    return "b $label"
  }
}

/**
 * BL is a branch with link instruction. It is similar to the branch instruction
 * except it copies the address of the next instruction (after the BL) into the
 * link register. BL is used for a subroutine call to branch back to the caller
 * based on the address value in the link register.
 *
 * @param label The label to jump to
 */
class BL(val label: Instruction): BranchInstruction() {
  override fun translate(): String {
    return "bl $label"
  }
}