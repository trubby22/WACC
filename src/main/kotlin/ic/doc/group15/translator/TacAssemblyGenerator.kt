package ic.doc.group15.translator

import ic.doc.group15.assembly.BranchLabel
import ic.doc.group15.assembly.LibraryFunction
import ic.doc.group15.assembly.LibraryFunction.Companion.MALLOC
import ic.doc.group15.assembly.UtilFunction.*
import ic.doc.group15.assembly.instruction.*
import ic.doc.group15.assembly.instruction.ConditionCode.*
import ic.doc.group15.assembly.operand.*
import ic.doc.group15.assembly.operand.ArmRegister.*
import ic.doc.group15.assembly.operand.Operand
import ic.doc.group15.ast.BinaryOp
import ic.doc.group15.ast.BinaryOp.*
import ic.doc.group15.ssa.*
import ic.doc.group15.ssa.cfg.CfgState
import ic.doc.group15.ssa.optimisations.*
import ic.doc.group15.ssa.tac.*
import java.lang.IllegalArgumentException

class TacAssemblyGenerator(
    cfg: ControlFlowGraph,
    val cfgState: CfgState,
    enableLogging: Boolean = false,
    val enableOptimisation: Boolean = false
) : AssemblyGenerator<SsaTranslatable>(cfg, enableLogging) {

    private val blockToLabelMap: MutableMap<BasicBlock, BranchLabel> = HashMap()

    @TranslatorMethod
    private fun translateCfg(cfg: ControlFlowGraph) {
        cfg.getFunctions().forEach { translate(it) }
    }

    private fun translateIRFunction(func: IRFunction) {
        val funcName = "f_" + func.funcAST.funcName
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

        if (enableOptimisation) {
            val instructions = block.getInstructionList()
            val optimiseStatements = instructions
                .map(OperationIdentity::apply)
                .map(ConstantFolding::apply)
                .map(OperatorStrengthReduction::apply)

            val constPropOpt = LocalConstantPropagation.apply(optimiseStatements)
            val removeRedunAssignOpt = RemoveRedundantAssignments.apply(constPropOpt)
            val removeTempOpt = RemoveTemporaries.apply(removeRedunAssignOpt)

            block.setInstructionList(removeTempOpt)
        }

        block.getInstructionList().forEach { translate(it) }
        return label
    }

    @TranslatorMethod
    private fun translateAssignBinOp(node: TacAssignBinOp) {
        val destReg = translateVar(node.dest)
        val xVar = moveToReg(node.x)
        val xReg = translateVar(xVar)
        val yOperand = translateOperand(node.y)
        when (node.op) {
            PLUS -> {
                defineUtilFuncs(P_THROW_OVERFLOW_ERROR)
                addLines(
                    Add(destReg, xReg, yOperand),
                    BranchLink(VS, P_THROW_OVERFLOW_ERROR)
                )
            }
            MINUS -> {
                defineUtilFuncs(P_THROW_OVERFLOW_ERROR)
                addLines(
                    Sub(destReg, xReg, yOperand),
                    BranchLink(VS, P_THROW_OVERFLOW_ERROR)
                )
            }
            MULT -> {
                defineUtilFuncs(P_THROW_OVERFLOW_ERROR)
                val yVar = moveToReg(node.y)
                val yReg = translateVar(yVar)
                addLines(
                    LongMult(updateFlags = true, xReg, yReg, xReg, yReg),
                    Compare(yReg, ArithmeticShiftRight(xReg, 31)),
                    BranchLink(NE, P_THROW_OVERFLOW_ERROR)
                )
            }
            DIV -> {
                defineUtilFuncs(P_CHECK_DIVIDE_BY_ZERO)
                val yVar = moveToReg(node.y)
                val yReg = translateVar(yVar)
                addLines(
                    Move(R0, xReg),
                    Move(R1, yReg),
                    BranchLink(P_CHECK_DIVIDE_BY_ZERO),
                    BranchLink(LibraryFunction.AEABI_IDIV),
                    Move(destReg, R0)
                )
            }
            MOD -> {
                defineUtilFuncs(P_CHECK_DIVIDE_BY_ZERO)
                val yVar = moveToReg(node.y)
                val yReg = translateVar(yVar)
                addLines(
                    Move(R0, xReg),
                    Move(R1, yReg),
                    BranchLink(P_CHECK_DIVIDE_BY_ZERO),
                    BranchLink(LibraryFunction.AEABI_IDIVMOD),
                    Move(destReg, R1)
                )
            }
            AND -> {
                val yVar = moveToReg(node.y)
                val yReg = translateVar(yVar)
                addLines(And(destReg, xReg, yReg))
            }
            OR -> {
                val yVar = moveToReg(node.y)
                val yReg = translateVar(yVar)
                addLines(Or(destReg, xReg, yReg))
            }
            else -> {
                val yVar = moveToReg(node.y)
                val yReg = translateVar(yVar)
                addLines(
                    Compare(xReg, yReg)
                )
                val (cond, condNeg) = when (node.op) {
                    BinaryOp.GT -> Pair(ConditionCode.GT, ConditionCode.LE)
                    GTE -> Pair(ConditionCode.GE, ConditionCode.LT)
                    BinaryOp.LT -> Pair(ConditionCode.LT, ConditionCode.GE)
                    LTE -> Pair(ConditionCode.LE, ConditionCode.GT)
                    EQUALS -> Pair(ConditionCode.EQ, ConditionCode.NE)
                    NOT_EQUALS -> Pair(ConditionCode.NE, ConditionCode.EQ)
                    else -> throw IllegalStateException()
                }
                addLines(
                    Move(cond, destReg, IntImmediateOperand(1)),
                    Move(condNeg, destReg, IntImmediateOperand(0))
                )
            }
        }
    }

    private fun moveToReg(operand: TacOperand): TacVar {
        return if (operand is TacVar) {
            operand
        } else {
            val newReg = cfgState.newVar(operand.type())
            translate(
                TacAssignValue(newReg, operand)
            )
            newReg
        }
    }

    @TranslatorMethod
    private fun translateAssignValue(node: TacAssignValue) {
        val v = translateVar(node.dest)
        var operand: Operand = translateOperand(node.x)
        if (operand is IntImmediateOperand) {
            operand = PseudoImmediateOperand(operand.value)
            addLines(LoadWord(v, operand))
        } else {
            addLines(Move(v, operand))
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
            addLines(LoadWord(R0, operand))
        } else {
            addLines(Move(R0, operand))
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
