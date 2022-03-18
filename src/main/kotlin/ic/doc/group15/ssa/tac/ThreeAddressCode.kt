package ic.doc.group15.ssa.tac

import ic.doc.group15.ast.BinaryOp
import ic.doc.group15.ssa.Successor

/**
 * Quadruple form - three address code maintains an unlimited amount of registers,
 * representing either declared variables or temporaries
 */
sealed interface ThreeAddressCode {
    /**
     * Set of variables used by the current instruction.
     */
    fun usesSet(): Set<Var>

    /**
     * Set of variables defined by the current instruction.
     */
    fun definesSet(): Set<Var>
}

/**
 * Assignments
 */
data class AssignBinOp(
    val reg: Var,
    val op: BinaryOp,
    val x: Operand,
    val y: Operand
) : ThreeAddressCode {
    override fun usesSet(): Set<Var> = setOf(x, y).filterIsInstance<Var>().toSet()
    override fun definesSet(): Set<Var> = setOf(reg)
}

data class AssignValue(
    val reg: Var,
    val x: Operand
) : ThreeAddressCode {
    override fun usesSet(): Set<Var> = setOf(x).filterIsInstance<Var>().toSet()
    override fun definesSet(): Set<Var> = setOf(reg)
}

class AssignCall(
    val reg: Var,
    val f: Func,
    vararg val args: Operand
) : ThreeAddressCode {
    override fun usesSet(): Set<Var> = args.filterIsInstance<Var>().toSet()
    override fun definesSet(): Set<Var> = setOf(reg)
}

/**
 * (Void) function calls
 */
class Call(
    val f: Func,
    vararg val args: Operand
) : ThreeAddressCode {
    override fun usesSet(): Set<Var> = args.filterIsInstance<Var>().toSet()
    override fun definesSet(): Set<Var> = emptySet()
}

/**
 * Branch statements
 */

data class BranchIf(
    val cond: Operand,
    val block: Successor
) : ThreeAddressCode {
    override fun usesSet(): Set<Var> = setOf(cond).filterIsInstance<Var>().toSet()
    override fun definesSet(): Set<Var> = emptySet()
}

data class Branch(
    val block: Successor
) : ThreeAddressCode {
    override fun usesSet(): Set<Var> = emptySet()
    override fun definesSet(): Set<Var> = emptySet()
}

/**
 * Memory instructions (used for heap allocations/value storing)
 */
data class Allocate(
    val reg: Var,
    val amount: Operand
) : ThreeAddressCode {
    override fun usesSet(): Set<Var> = setOf(amount).filterIsInstance<Var>().toSet()
    override fun definesSet(): Set<Var> = setOf(reg)
}

data class Load(
    val reg: Var,
    val x: Operand
) : ThreeAddressCode {
    override fun usesSet(): Set<Var> = setOf(x).filterIsInstance<Var>().toSet()
    override fun definesSet(): Set<Var> = setOf(reg)
}

data class Store(
    val reg: Var,
    val x: Operand
) : ThreeAddressCode {
    override fun usesSet(): Set<Var> = setOf(x).filterIsInstance<Var>().toSet()
    override fun definesSet(): Set<Var> = setOf(reg)
}

data class Argument(
    val reg: Var
) : ThreeAddressCode {
    override fun usesSet(): Set<Var> = emptySet()
    override fun definesSet(): Set<Var> = setOf(reg)
}

///**
// * Pseudo-instruction for SSA form
// */
//data class Phi(val reg: Var, val args: Collection<Operand>) : ThreeAddressCode
