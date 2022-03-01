package ic.doc.group15.codegen.assembly.instruction

import ic.doc.group15.codegen.assembly.Instruction
import ic.doc.group15.codegen.assembly.operand.Register
import ic.doc.group15.codegen.assembly.operand.Operand

/**
 * Comparison operations present in ARM1176JZF-S, partly implemented.
 */
abstract class CompareInstruction protected constructor(
    instr: String,
    val base: Register,
    val op: Operand
) : Instruction(instr, base, op)

/**
 * CMP is a compare instruction that compares the value in the register with the
 * operand by subtracting the value of operand from the value in the register,
 * then update the condition flags N,Z,C,V on the result, but do not place the
 * result in any register. It is similar to a call to the SUBS instruction except
 * that the result is discarded.
 *
 * @param base The base register/register holding the first operand
 * @param op A flexible second operand
 */
class Compare(base: Register, op: Operand) : CompareInstruction("cmp", base, op)
