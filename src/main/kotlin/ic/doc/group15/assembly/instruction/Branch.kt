package ic.doc.group15.assembly.instruction

import ic.doc.group15.assembly.Instruction
import ic.doc.group15.assembly.UtilFunction
import ic.doc.group15.assembly.operand.BranchLabelOperand

/**
 * Branch operations present in ARM1176JZF-S, partly implemented.
 */
abstract class BranchInstruction protected constructor(
    instr: String,
    conditionCode: ConditionCode?,
    val labelOp: BranchLabelOperand
) : Instruction(instr, conditionCode, labelOp)

/**
 * B is a branch instruction. Similar to JMP in x86, B causes a branch to label.
 *
 * @param label The label to jump to
 */
class Branch(
    conditionCode: ConditionCode?,
    labelOp: BranchLabelOperand
) : BranchInstruction("B", conditionCode, labelOp) {

    constructor(labelOp: BranchLabelOperand) : this(null, labelOp)

    constructor(conditionCode: ConditionCode?, utilFunc: UtilFunction) :
            this(conditionCode, BranchLabelOperand(utilFunc.labelBlock))
    constructor(utilFunc: UtilFunction) : this(null, utilFunc)
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
    labelOp: BranchLabelOperand
) : BranchInstruction("BL", conditionCode, labelOp) {

    constructor(labelOp: BranchLabelOperand) : this(null, labelOp)

    constructor(conditionCode: ConditionCode?, utilFunc: UtilFunction) :
        this(conditionCode, BranchLabelOperand(utilFunc.labelBlock))
    constructor(utilFunc: UtilFunction) : this(null, utilFunc)
}
