package ic.doc.group15.ssa

import ic.doc.group15.ast.*
import ic.doc.group15.ssa.tac.ThreeAddressCode
import ic.doc.group15.ssa.tac.TacVar

/**
 * A function contains a list of basic blocks forming the control flow graph (CFG) for
 * the function.
 */
class IRFunction(val funcAST: FunctionDeclarationAST) : SsaTranslatable {
    val entryBlock = EntryBasicBlock(*funcAST.formals.toTypedArray())

    // not strictly needed since can be obtained from entryBlock, but handy to have
    val basicBlocks = mutableListOf<BasicBlock>()
    val exitBlock = ExitBasicBlock()

    lateinit var variableSet: Set<TacVar>

    override fun toString(): String {
        return funcAST.funcName
    }

    fun addBlocks(vararg blocks: BasicBlock) {
        basicBlocks.addAll(blocks)
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

interface Successor {

    fun addPredecessors(vararg predecessors: Predecessor)

    fun getPredecessors(): List<Predecessor>
}

interface Predecessor {

    fun addSuccessors(vararg successors: Successor)

    fun getSuccessors(): List<Successor>
}

open class SuccessorBlock : Successor {

    private val predecessors = LinkedHashSet<Predecessor>()

    override fun addPredecessors(vararg predecessors: Predecessor) {
        this.predecessors.addAll(predecessors)
        predecessors.forEach { pred -> pred.addSuccessors(this) }
    }

    override fun getPredecessors(): List<Predecessor> = predecessors.toList()
}

open class PredecessorBlock : Predecessor {

    private val successors = LinkedHashSet<Successor>()

    override fun addSuccessors(vararg successors: Successor) {
        successors.forEach { it.addPredecessors(this) }
        this.successors.addAll(successors)
    }

    override fun getSuccessors(): List<Successor> = successors.toList()
}

abstract class BidirectionalBlock : Successor, Predecessor {

    abstract override fun addPredecessors(vararg predecessors: Predecessor)

    abstract override fun getPredecessors(): List<Predecessor>

    abstract override fun addSuccessors(vararg successors: Successor)

    abstract override fun getSuccessors(): List<Successor>
}

/**
 * The entry basic block in a function has two characteristics:
 * 1) It is immediately executed on entrance to the function;
 * 2) It is not allowed to have predecessor basic blocks (hence no Phi nodes)
 */
class EntryBasicBlock(vararg val arguments: ParameterAST) : PredecessorBlock()

/**
 * The exit basic block in a function has one characteristic:
 * It is not allowed to have successor basic blocks.
 */
class ExitBasicBlock : SuccessorBlock()

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
class BasicBlock(val irFunction: IRFunction) : BidirectionalBlock(), SsaTranslatable {

    private var instructions = mutableListOf<ThreeAddressCode>()

    // control flow analysis
    private val predecessors = LinkedHashSet<Predecessor>()
    private val successors = LinkedHashSet<Successor>()

    init {
        irFunction.addBlocks(this)
    }

    fun addInstructions(vararg instructions: ThreeAddressCode) {
        this.instructions.addAll(instructions)
    }

    fun getInstructionList(): List<ThreeAddressCode> = instructions

    fun setInstructionList(instructions: List<ThreeAddressCode>) {
        this.instructions = instructions.toMutableList()
    }

    override fun addPredecessors(vararg predecessors: Predecessor) {
        this.predecessors.addAll(predecessors)
        predecessors.forEach { pred -> pred.addSuccessors(this) }
    }

    override fun getPredecessors(): List<Predecessor> = predecessors.toList()

    override fun addSuccessors(vararg successors: Successor) {
        successors.forEach { it.addPredecessors(this) }
        this.successors.addAll(successors)
    }

    override fun getSuccessors(): List<Successor> = successors.toList()
}
