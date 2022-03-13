package ic.doc.group15.ssa

import ic.doc.group15.assembly.UniqueLabelGenerator
import ic.doc.group15.ast.*
import java.util.concurrent.atomic.AtomicInteger

open class Register {
    companion object {
        private val counter = AtomicInteger()

        fun reset() {
            counter.set(0)
        }
    }

    val id = counter.getAndIncrement()

    override fun toString(): String {
        return "%$id"
    }
}

/**
 * A function contains a list of basic blocks forming the control flow graph (CFG) for
 * the function.
 */
class Function(val funcAST: FunctionDeclarationAST) {
    val labelGenerator = UniqueLabelGenerator("B")
    val entryBlock = EntryBasicBlock(*funcAST.formals.toTypedArray())
    // not strictly needed since can be obtained from entryBlock, but handy to have
    val basicBlocks = mutableListOf<BasicBlock>()

    override fun toString(): String {
        return funcAST.funcName
    }

    fun printCode(): String {
        TODO()
    }
}

/**
 * The entry basic block in a function has two characteristics:
 * 1) It is immediately executed on entrance to the function;
 * 2) It is not allowed to have predecessor basic blocks (hence no Phi nodes)
 */
class EntryBasicBlock(vararg arguments: ParameterAST) {
    val instructions = listOf(*arguments)
    private val successors = mutableListOf<BasicBlock>()

    fun addSuccessors(vararg successors: BasicBlock) {
        this.successors.addAll(successors)
    }
}

/**
 * A basic block is a straight-line code sequence with no branches in except to the
 * entry and no branches out except at the exit.
 *
 * It acts as a node in the control flow graph (CFG) for the function,
 * where it contains a list of instructions and a terminator instruction (a branch
 * or return statement).
 *
 * It has one entry point and one exit point, hence no instructions within a basic block
 * is the destination of a jump instruction anywhere in the program except for the
 * terminator instruction, where it may lead the program to execute instructions in a
 * different basic block. This defines the execution behaviour such that every instruction
 * in a basic block is executed in sequence.
 **/
class BasicBlock(val function: Function) {

    private val label = function.labelGenerator.generate()
    private val phis = mutableListOf<PhiAST>()
    private val instructions = mutableListOf<StatementAST>()
    private val predecessors = mutableListOf<BasicBlock>()
    private val successors = mutableListOf<BasicBlock>()

    override fun toString(): String {
        return label
    }

    fun addInstructions(vararg instructions: StatementAST) {
        this.instructions.addAll(instructions)
    }

    fun addPredecessors(vararg predecessors: BasicBlock) {
        this.predecessors.addAll(predecessors)
    }

    fun addSuccessors(vararg successors: BasicBlock) {
        this.successors.addAll(successors)
    }
}