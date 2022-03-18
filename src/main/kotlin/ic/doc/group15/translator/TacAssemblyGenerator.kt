package ic.doc.group15.translator

import ic.doc.group15.assembly.BranchLabel
import ic.doc.group15.assembly.LibraryFunction.Companion.MALLOC
import ic.doc.group15.assembly.instruction.*
import ic.doc.group15.assembly.operand.*
import ic.doc.group15.assembly.operand.ArmRegister.R0
import ic.doc.group15.assembly.operand.Operand
import ic.doc.group15.ssa.*
import ic.doc.group15.ssa.tac.*
import java.lang.IllegalArgumentException

class TacAssemblyGenerator(
    cfg: ControlFlowGraph,
    enableLogging: Boolean = false
) : AssemblyGenerator<SsaTranslatable>(cfg, enableLogging) {

    private val blockToLabelMap: MutableMap<BasicBlock, BranchLabel> = HashMap()

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

    private fun translateBasicBlock(block: BasicBlock, name: String? = null): BranchLabel {
        val label = if (name != null) {
            newBranchLabel(name)
        } else {
            newBranchLabel()
        }
        blockToLabelMap[block] = label
        currentLabel = label
        block.getInstructionList().forEach { translate(it) }
        return label
    }

    @TranslatorMethod
    private fun translateAssignBinOp(node: TacAssignBinOp) {

    }

    @TranslatorMethod
    private fun translateAssignValue(node: TacAssignValue) {
        val v = translateVar(node.dest)
        var operand: Operand = translateOperand(node.x)
        if (operand is IntImmediateOperand) {
            operand = PseudoImmediateOperand(operand.value)
            addLines(
                LoadWord(v, operand)
            )
        } else {
            addLines(
                Move(v, operand)
            )
        }
    }

    @TranslatorMethod
    private fun translateAssignCall(node: TacAssignCall) {

    }

    @TranslatorMethod
    private fun translateCall(node: TacCall) {
        
    }

    @TranslatorMethod
    private fun translateBranchIf(node: TacBranchIf) {

    }

    @TranslatorMethod
    private fun translateBranch(node: TacBranch) {
        val label = blockToLabelMap.computeIfAbsent(node.block) {
            translateBasicBlock(it)
        }
        addLines(
            Branch(BranchLabelOperand(label))
        )
    }

    @TranslatorMethod
    private fun translateAllocate(node: TacAllocate) {
        val v = translateVar(node.reg)
        var operand: Operand = translateOperand(node.amount)
        if (operand is IntImmediateOperand) {
            operand = PseudoImmediateOperand(operand.value)
            addLines(
                LoadWord(R0, operand)
            )
        } else {
            addLines(
                Move(R0, operand)
            )
        }
        addLines(
            Branch(MALLOC),
            Move(v, R0)
        )
    }

    @TranslatorMethod
    private fun translateLoad(node: TacLoad) {
        addLines(
            LoadWord(translateVar(node.dest), translateOperand(node.src) as AddressOperand)
        )
    }

    @TranslatorMethod
    private fun translateStore(node: TacStore) {
        addLines(
            StoreWord(translateVar(node.dest), translateOperand(node.src) as AddressOperand)
        )
    }

    private fun translateOperand(op: TacOperand): Operand {
        return when (op) {
            is CharImm -> translateCharImm(op)
            is IntImm -> translateIntImm(op)
            is BoolImm -> translateBoolImm(op)
            is StrImm -> translateStrImm(op)
            is TacVar -> translateVar(op)
            else -> throw IllegalArgumentException()
        }
    }

    private fun translateCharImm(op: CharImm): Operand {
        return CharImmediateOperand(op.value)
    }

    private fun translateIntImm(op: IntImm): Operand {
        return IntImmediateOperand(op.value)
    }

    private fun translateBoolImm(op: BoolImm): Operand {
        return BoolImmediateOperand(op.value)
    }

    private fun translateStrImm(op: StrImm): Operand {
        val stringLabel = newStringLabel(op.value)
        return DataLabelOperand(stringLabel)
    }

    private fun translateVar(op: TacVar): Register {
        return PseudoRegister(op.id)
    }
}
