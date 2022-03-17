package ic.doc.group15.ssa.tac

import ic.doc.group15.ast.BinaryOp
import ic.doc.group15.ssa.Successor

/**
 * Quadruple form - three address code maintains an unlimited amount of registers,
 * representing either declared variables or temporaries
 */
sealed interface ThreeAddressCode

/**
 * Assignments
 */
data class AssignBinOp(
    val reg: Var,
    val op: BinaryOp,
    val x: Operand,
    val y: Operand
) : ThreeAddressCode

data class AssignValue(
    val reg: Var,
    val x: Operand
) : ThreeAddressCode

class AssignCall(
    val reg: Var,
    val f: Func,
    vararg val args: Operand
) : ThreeAddressCode

/**
 * (Void) function calls
 */
class Call(
    val f: Func,
    vararg val args: Operand
) : ThreeAddressCode

/**
 * Branch statements
 */

data class BranchIf(
    val cond: Operand,
    val block: Successor
) : ThreeAddressCode

data class Branch(
    val block: Successor
) : ThreeAddressCode

/**
 * Memory instructions (used for heap allocations/value storing)
 */
data class Allocate(
    val reg: Var,
    val amount: Operand
) : ThreeAddressCode

data class Load(
    val reg: Var,
    val x: Operand
) : ThreeAddressCode

data class Store(
    val reg: Var,
    val x: Operand
) : ThreeAddressCode

data class Argument(
    val reg: Var
) : ThreeAddressCode

/**
 * Pseudo-instruction for SSA form
 */
data class Phi(val reg: Var, val args: Collection<Operand>) : ThreeAddressCode
