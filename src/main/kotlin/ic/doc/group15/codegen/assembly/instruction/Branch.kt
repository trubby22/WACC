package ic.doc.group15.codegen.assembly.instruction

import ic.doc.group15.codegen.assembly.BranchLabel
import ic.doc.group15.codegen.assembly.Instruction
import ic.doc.group15.codegen.assembly.operand.LabelOperand

/**
 * Branch operations present in ARM1176JZF-S, partly implemented.
 */
abstract class BranchInstruction protected constructor(
    instr: String,
    conditionCode: ConditionCode?,
    val labelName: String
) : Instruction(instr, conditionCode, LabelOperand(labelName))

/**
 * B is a branch instruction. Similar to JMP in x86, B causes a branch to label.
 *
 * @param label The label to jump to
 */
class Branch(
    conditionCode: ConditionCode?,
    label: String
) : BranchInstruction("b", conditionCode, label) {

    constructor(label: String) : this(null, label)

    constructor(conditionCode: ConditionCode?, label: BranchLabel) : this(conditionCode, label.name)
    constructor(label: BranchLabel) : this(null, label)
}

/**
 * BL is a branch with link instruction. It is similar to the branch instruction
 * except it copies the address of the next instruction (after the BL) into the
 * link register. BL is used for a subroutine call to branch back to the caller
 * based on the address value in the link register.
 *
 * @param label The label to jump to
 */
class BranchLink(
    conditionCode: ConditionCode?,
    label: String,
) : BranchInstruction("bl", conditionCode, label) {

    constructor(label: String) : this(null, label)

    constructor(conditionCode: ConditionCode?, label: BranchLabel) : this(conditionCode, label.name)
    constructor(label: BranchLabel) : this(null, label)
}
