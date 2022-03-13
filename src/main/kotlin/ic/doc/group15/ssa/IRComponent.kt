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
class IRFunction(val funcAST: FunctionDeclarationAST) {
    private val labelGenerator = UniqueLabelGenerator("B")
    val entryBlock = EntryBasicBlock(*funcAST.formals.toTypedArray())
    // not strictly needed since can be obtained from entryBlock, but handy to have
    val basicBlocks = mutableListOf<BasicBlock>()
    val exitBlock = ExitBasicBlock()

    override fun toString(): String {
        return funcAST.funcName
    }

    fun addBlocks(vararg blocks: BasicBlock) {
        basicBlocks.addAll(blocks)
    }

    fun makeLabel(): String {
        return labelGenerator.generate()
    }

    fun sealBlock() {
        if (basicBlocks.isEmpty()) {
            entryBlock.addSuccessors(exitBlock)
        } else {
            val lastStat = basicBlocks.last()
            lastStat.addSuccessors(exitBlock)
        }
    }

    fun printCode(): String {
        TODO()
    }
}

interface SuccessorBlock {
    fun addPredecessors(vararg predecessors: PredecessorBlock)
}

interface PredecessorBlock {
    fun addSuccessors(vararg successors: SuccessorBlock)
}

interface Block: SuccessorBlock, PredecessorBlock

/**
 * The entry basic block in a function has two characteristics:
 * 1) It is immediately executed on entrance to the function;
 * 2) It is not allowed to have predecessor basic blocks (hence no Phi nodes)
 */
class EntryBasicBlock(vararg arguments: ParameterAST): PredecessorBlock {
    val arguments = listOf(*arguments)
    private val successors = mutableSetOf<SuccessorBlock>()

    override fun addSuccessors(vararg successors: SuccessorBlock) {
        this.successors.addAll(successors)
        successors.forEach { succ -> succ.addPredecessors(this) }
    }
}

/**
 * The exit basic block in a function has one characteristic:
 * It is not allowed to have successor basic blocks.
 */
class ExitBasicBlock: SuccessorBlock {
    private val predecessors = mutableSetOf<PredecessorBlock>()

    override fun addPredecessors(vararg predecessors: PredecessorBlock) {
        this.predecessors.addAll(predecessors)
        predecessors.forEach { pred -> pred.addSuccessors(this) }
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
class BasicBlock(val irFunction: IRFunction): Block {

    private val label = irFunction.makeLabel()
    private val phis = mutableListOf<PhiAST>()
    private val instructions = mutableListOf<ASTNode>()
    private val predecessors = mutableSetOf<PredecessorBlock>()
    private val successors = mutableSetOf<SuccessorBlock>()

    init {
        irFunction.addBlocks(this)
    }

    override fun toString(): String {
        return label
    }

    fun addPhis(vararg instructions: PhiAST) {
        this.phis.addAll(instructions)
    }

    fun addInstructions(vararg instructions: ASTNode) {
        this.instructions.addAll(instructions)
    }

    override fun addPredecessors(vararg predecessors: PredecessorBlock) {
        this.predecessors.addAll(predecessors)
        predecessors.forEach { pred -> pred.addSuccessors(this) }
    }

    override fun addSuccessors(vararg successors: SuccessorBlock) {
        this.successors.addAll(successors)
        successors.forEach { succ -> succ.addPredecessors(this) }
    }
}