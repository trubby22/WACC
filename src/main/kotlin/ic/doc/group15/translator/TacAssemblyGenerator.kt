package ic.doc.group15.translator

import ic.doc.group15.ssa.BasicBlock
import ic.doc.group15.ssa.ControlFlowGraph
import ic.doc.group15.ssa.IRFunction
import ic.doc.group15.ssa.SsaTranslatable
import ic.doc.group15.ssa.tac.*

class TacAssemblyGenerator(
    cfg: ControlFlowGraph,
    enableLogging: Boolean = false
) : AssemblyGenerator<SsaTranslatable>(cfg, enableLogging) {

    @TranslatorMethod
    private fun translateCfg(cfg: ControlFlowGraph) {
        cfg.getFunctions().forEach { translate(it) }
    }

    private fun translateIRFunction(func: IRFunction) {
        val funcName = func.funcAST.funcName
        translateBasicBlock(func.basicBlocks.first(), funcName)
        for (block in func.basicBlocks.drop(1)) {
            translateBasicBlock(block)
        }
    }

    private fun translateBasicBlock(block: BasicBlock, name: String? = null) {
        val label = if (name != null) {
            newBranchLabel(name)
        } else {
            newBranchLabel()
        }
        currentLabel = label
        block.getInstructionList().forEach { translate(it) }
    }

    @TranslatorMethod
    private fun translateAssignBinOp(node: AssignBinOp) {

    }

    @TranslatorMethod
    private fun translateAssignValue(node: AssignValue) {

    }

    @TranslatorMethod
    private fun translateAssignCall(node: AssignCall) {

    }

    @TranslatorMethod
    private fun translateCall(node: Call) {

    }

    @TranslatorMethod
    private fun translateBranchIf(node: BranchIf) {

    }

    @TranslatorMethod
    private fun translateBranch(node: Branch) {

    }

    @TranslatorMethod
    private fun translateAllocate(node: Allocate) {

    }

    @TranslatorMethod
    private fun translateLoad(node: Load) {

    }

    @TranslatorMethod
    private fun translateStore(node: Store) {

    }

    @TranslatorMethod
    private fun translateArgument(node: Argument) {

    }
}
