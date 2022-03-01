package ic.doc.group15.codegen.assembly.instruction

import ic.doc.group15.codegen.assembly.Instruction
import ic.doc.group15.codegen.assembly.operand.ConditionCode
import ic.doc.group15.codegen.assembly.operand.LabelOperand

/**
 * Branch operations present in ARM1176JZF-S, partly implemented.
 */
abstract class BranchInstruction protected constructor(
    instr: String,
    val labelName: String
) : Instruction(instr, LabelOperand(labelName))

/**
 * B is a branch instruction. Similar to JMP in x86, B causes a branch to label.
 *
 * @param cond Optional condition code, which only allows execution of instruction if true
 * @param label The label to jump to
 */
class Branch(cond: ConditionCode? = null, label: String) : BranchInstruction("b${cond?.toString() ?: ""}", label)

/**
 * BL is a branch with link instruction. It is similar to the branch instruction
 * except it copies the address of the next instruction (after the BL) into the
 * link register. BL is used for a subroutine call to branch back to the caller
 * based on the address value in the link register.
 *
 * @param cond Optional condition code, which only allows execution of instruction if true
 * @param label The label to jump to
 */
class BranchLink(cond: ConditionCode? = null, label: String) : BranchInstruction("bl${cond?.toString() ?: ""}", label)
