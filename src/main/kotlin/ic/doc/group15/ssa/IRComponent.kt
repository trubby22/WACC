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
            // Add edge from entry point to first block
            val firstStat = basicBlocks.first()
            firstStat.addPredecessors(entryBlock)
            // Add edge from last block to exit point
            val lastStat = basicBlocks.last()
            lastStat.addSuccessors(exitBlock)
        }
    }

    fun printCode(): String {
        TODO()
    }
}

interface Block {
    // Insertion-ordered.
    fun getSuccessors(): List<Block>?
    // Insertion-ordered.
    fun getPredecessors(): List<Block>?
}

interface SuccessorBlock : Block {
    fun addPredecessors(vararg predecessors: PredecessorBlock)
}

interface PredecessorBlock : Block {
    fun addSuccessors(vararg successors: SuccessorBlock)
}

interface BidirectionalBlock: SuccessorBlock, PredecessorBlock

/**
 * The entry basic block in a function has two characteristics:
 * 1) It is immediately executed on entrance to the function;
 * 2) It is not allowed to have predecessor basic blocks (hence no Phi nodes)
 */
class EntryBasicBlock(vararg arguments: ParameterAST): PredecessorBlock {
    val arguments = listOf(*arguments)
    private val successors = LinkedHashSet<SuccessorBlock>()

    override fun addSuccessors(vararg successors: SuccessorBlock) {
        this.successors.addAll(successors)
        successors.forEach { succ -> succ.addPredecessors(this) }
    }

    override fun getSuccessors(): List<Block> {
        return successors.toList()
    }

    override fun getPredecessors(): List<Block>? {
        return null
    }
}

/**
 * The exit basic block in a function has one characteristic:
 * It is not allowed to have successor basic blocks.
 */
class ExitBasicBlock: SuccessorBlock {
    private val predecessors = LinkedHashSet<PredecessorBlock>()

    override fun addPredecessors(vararg predecessors: PredecessorBlock) {
        this.predecessors.addAll(predecessors)
        predecessors.forEach { pred -> pred.addSuccessors(this) }
    }

    override fun getSuccessors(): List<Block>? {
        return null
    }

    override fun getPredecessors(): List<Block> {
        return predecessors.toList()
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
class BasicBlock(val irFunction: IRFunction): BidirectionalBlock {

    private val label = irFunction.makeLabel()
    private val phis = mutableListOf<PhiAST>()
    private val instructions = mutableListOf<ASTNode>()
    // control flow analysis
    private val predecessors = LinkedHashSet<PredecessorBlock>()
    private val successors = LinkedHashSet<SuccessorBlock>()

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

    override fun getSuccessors(): List<Block> {
        return successors.toList()
    }

    override fun getPredecessors(): List<Block> {
        return predecessors.toList()
    }

    override fun addSuccessors(vararg successors: SuccessorBlock) {
        this.successors.addAll(successors)
        successors.forEach { succ -> succ.addPredecessors(this) }
    }
}