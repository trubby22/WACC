package ic.doc.group15.ssa.tac

import ic.doc.group15.ast.BinaryOp
import ic.doc.group15.ssa.BasicBlock
import ic.doc.group15.ssa.SsaTranslatable

/**
 * Quadruple form - three address code maintains an unlimited amount of registers,
 * representing either declared variables or temporaries
 */
sealed interface ThreeAddressCode : SsaTranslatable {

    /**
     * Set of variables used by the current instruction.
     */
    fun usesSet(): Set<TacVar>

    /**
     * Set of variables defined by the current instruction.
     */
    fun definesSet(): Set<TacVar>
}

/**
 * Assignments
 */
data class TacAssignBinOp(
    val dest: TacVar,
    val op: BinaryOp,
    val x: TacOperand,
    val y: TacOperand
) : ThreeAddressCode {
    override fun usesSet(): Set<TacVar> = setOf(x, y).filterIsInstance<TacVar>().toSet()
    override fun definesSet(): Set<TacVar> = setOf(dest)
}

data class TacAssignValue(
    val dest: TacVar,
    val x: TacOperand
) : ThreeAddressCode {
    override fun usesSet(): Set<TacVar> = setOf(x).filterIsInstance<TacVar>().toSet()
    override fun definesSet(): Set<TacVar> = setOf(dest)
}

class TacAssignCall(
    val dest: TacVar,
    val f: Func,
    vararg val args: TacOperand
) : ThreeAddressCode {
    override fun usesSet(): Set<TacVar> = args.filterIsInstance<TacVar>().toSet()
    override fun definesSet(): Set<TacVar> = setOf(dest)
}

/**
 * (Void) function calls
 */
class TacCall(
    val f: Func,
    vararg val args: TacOperand
) : ThreeAddressCode {
    override fun usesSet(): Set<TacVar> = args.filterIsInstance<TacVar>().toSet()
    override fun definesSet(): Set<TacVar> = emptySet()
}

/**
 * Branch statements
 */

data class TacBranchIf(
    val cond: TacOperand,
    val block: BasicBlock
) : ThreeAddressCode {
    override fun usesSet(): Set<TacVar> = setOf(cond).filterIsInstance<TacVar>().toSet()
    override fun definesSet(): Set<TacVar> = emptySet()
}

data class TacBranch(
    val block: BasicBlock
) : ThreeAddressCode {
    override fun usesSet(): Set<TacVar> = emptySet()
    override fun definesSet(): Set<TacVar> = emptySet()
}

/**
 * Memory instructions (used for heap allocations/value storing)
 */
data class TacAllocate(
    val reg: TacVar,
    val amount: TacOperand
) : ThreeAddressCode {
    override fun usesSet(): Set<TacVar> = setOf(amount).filterIsInstance<TacVar>().toSet()
    override fun definesSet(): Set<TacVar> = setOf(reg)
}

data class TacLoad(
    val dest: TacVar,
    val src: TacOperand
) : ThreeAddressCode {
    override fun usesSet(): Set<TacVar> = setOf(src).filterIsInstance<TacVar>().toSet()
    override fun definesSet(): Set<TacVar> = setOf(dest)
}

data class TacStore(
    val dest: TacVar,
    val src: TacOperand
) : ThreeAddressCode {
    override fun usesSet(): Set<TacVar> = setOf(src).filterIsInstance<TacVar>().toSet()
    override fun definesSet(): Set<TacVar> = setOf(dest)
}

data class Argument(
    val reg: TacVar
) : ThreeAddressCode {
    override fun usesSet(): Set<TacVar> = emptySet()
    override fun definesSet(): Set<TacVar> = setOf(reg)
}
