package ic.doc.group15.codegen.assembly.instruction

import ic.doc.group15.codegen.assembly.operand.Operand
import ic.doc.group15.codegen.assembly.operand.Register

abstract class MoveInstruction protected constructor(
    instr: String,
    conditionCode: ConditionCode?,
    updateFlags: Boolean,
    val dest: Register,
    val op: Operand
) : UpdateFlagsInstruction(instr, conditionCode, updateFlags, dest, op)

class Move(
    conditionCode: ConditionCode?,
    dest: Register,
    op: Operand,
    updateFlags: Boolean = false
) : UpdateFlagsInstruction("MOV", conditionCode, updateFlags, dest, op) {

    constructor(dest: Register, op: Operand, updateFlags: Boolean = false) :
        this(null, dest, op, updateFlags)
}

class MoveNot(
    conditionCode: ConditionCode?,
    dest: Register,
    op: Operand,
    updateFlags: Boolean = false
) : UpdateFlagsInstruction("MVN", conditionCode, updateFlags, dest, op) {

    constructor(dest: Register, op: Operand, updateFlags: Boolean = false) :
        this(null, dest, op, updateFlags)
}
