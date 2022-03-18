package ic.doc.group15.translator

import ic.doc.group15.assembly.BranchLabel
import ic.doc.group15.assembly.LibraryFunction
import ic.doc.group15.assembly.LibraryFunction.Companion.MALLOC
import ic.doc.group15.assembly.UtilFunction.*
import ic.doc.group15.assembly.UtilFunction
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
import ic.doc.group15.type.ArrayType
import ic.doc.group15.type.BasicType
import ic.doc.group15.type.PairType
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
        val destReg = translateVar(node.dest)
        when (node.f) {
            Functions.BANG -> {
                val arg = node.args[0]
                val operand = translateOperand(arg)
                if (operand is PseudoRegister) {
                    addLines(Xor(destReg, operand, IntImmediateOperand(1)))
                } else {
                    translate(TacAssignValue(node.dest, arg))
                    addLines(Xor(destReg, destReg, IntImmediateOperand(1)))
                }
            }
            Functions.LEN -> {
                val arg = node.args[0]
                val operand = translateOperand(node.args[0])
                if (operand is PseudoRegister) {
                    addLines(LoadWord(destReg, ZeroOffset(operand)))
                } else {
                    translate(TacAssignValue(node.dest, arg))
                    addLines(LoadWord(destReg, ZeroOffset(destReg)))
                }
            }
            Functions.LSL -> {
                val num = node.args[0]
                val operand = translateOperand(num)
                val bits = node.args[1]
                assert(bits is IntImm)
                if (operand is PseudoRegister) {
                    addLines(Move(destReg, LogicalShiftLeft(operand, (bits as IntImm).value)))
                } else {
                    translate(TacAssignValue(node.dest, num))
                    addLines(Move(destReg, LogicalShiftLeft(destReg, (bits as IntImm).value)))
                }
            }
            Functions.ASR -> {
                val num = node.args[0]
                val operand = translateOperand(num)
                val bits = node.args[1]
                assert(bits is IntImm)
                if (operand is PseudoRegister) {
                    addLines(Move(destReg, ArithmeticShiftRight(operand, (bits as IntImm).value)))
                } else {
                    translate(TacAssignValue(node.dest, num))
                    addLines(Move(destReg, ArithmeticShiftRight(destReg, (bits as IntImm).value)))
                }
            }
            else -> {}
        }
    }

    @TranslatorMethod
    private fun translateCall(node: TacCall) {
        when (node.f) {
            Functions.EXIT -> {
                addLines(
                    Move(R0, translateOperand(node.args[0])),
                    BranchLink(LibraryFunction.EXIT)
                )
            }
            Functions.RETURN -> {
                addLines(Move(R0, translateOperand(node.args[0])))
                // TODO unwind stack
                addLines(Pop(ArmRegister.PC))
            }
            Functions.READ -> {
                val targetReg = node.args[0]
                val readFunc =
                    if (targetReg.type() == BasicType.IntType) UtilFunction.P_READ_INT else UtilFunction.P_READ_CHAR
                defineUtilFuncs(readFunc)
                addLines(
                    Move(R0, translateOperand(targetReg)),
                    BranchLink(readFunc)
                )
            }
            Functions.PRINT -> {
                when (val type = node.args[0].type()) {
                    BasicType.StringType -> {
                        defineUtilFuncs(UtilFunction.P_PRINT_STRING)
                        addLines(BranchLink(UtilFunction.P_PRINT_STRING))
                    }
                    BasicType.CharType -> {
                        addLines(BranchLink(LibraryFunction.PUTCHAR))
                    }
                    BasicType.IntType -> {
                        defineUtilFuncs(UtilFunction.P_PRINT_INT)
                        addLines(BranchLink(UtilFunction.P_PRINT_INT))
                    }
                    BasicType.BoolType -> {
                        defineUtilFuncs(UtilFunction.P_PRINT_BOOL)
                        addLines(BranchLink(UtilFunction.P_PRINT_BOOL))
                    }
                    is PairType -> {
                        defineUtilFuncs(UtilFunction.P_PRINT_REFERENCE)
                        addLines(BranchLink(UtilFunction.P_PRINT_REFERENCE))
                    }
                    is ArrayType -> {
                        if (type.elementType == BasicType.CharType) {
                            defineUtilFuncs(UtilFunction.P_PRINT_STRING)
                            addLines(BranchLink(UtilFunction.P_PRINT_STRING))
                        } else {
                            defineUtilFuncs(UtilFunction.P_PRINT_REFERENCE)
                            addLines(BranchLink(UtilFunction.P_PRINT_REFERENCE))
                        }
                    }
                }
            }
            Functions.PRINTLN -> {
                val printTAC = TacCall(Functions.PRINT, node.args[0])
                translate(printTAC)
                defineUtilFuncs(UtilFunction.P_PRINT_LN)
                addLines(BranchLink(UtilFunction.P_PRINT_LN))
            }
            Functions.FREE -> {
                defineUtilFuncs(UtilFunction.P_FREE_PAIR)
                addLines(
                    Move(R0, translateOperand(node.args[0])),
                    BranchLink(UtilFunction.P_FREE_PAIR)
                )
            }
            else -> {}
        }
    }

    @TranslatorMethod
    private fun translateBranchIf(node: TacBranchIf) {
    }

    @TranslatorMethod
    private fun translateBranch(node: TacBranch) {
        val label = blockToLabelMap.computeIfAbsent(node.block) {
            translateBasicBlock(it)
        }
        addLines(Branch(BranchLabelOperand(label)))
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
