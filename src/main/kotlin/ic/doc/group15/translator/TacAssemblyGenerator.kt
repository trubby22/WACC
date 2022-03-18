package ic.doc.group15.translator

import ic.doc.group15.assembly.BranchLabel
import ic.doc.group15.assembly.Instruction
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

open class TacAssemblyGenerator(
    cfg: ControlFlowGraph,
    val cfgState: CfgState,
    enableLogging: Boolean = false,
    val enableOptimisation: Boolean = false
) : AssemblyGenerator<SsaTranslatable>(cfg, enableLogging) {

    private val blockToLabelMap: MutableMap<BasicBlock, BranchLabel> = LinkedHashMap()

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

        // Allocate actual registers
        val availableReg = ArrayDeque(listOf(R4, R5, R6, R7, R8, R9, R10, R11))
        val regAllocMap = LinearScanRegAlloc.apply(blockToLabelMap, availableReg)

        for ((block, label) in blockToLabelMap) {
            // Replace pseudo register with arm registers
            val instList = mutableListOf<Instruction>()
            for (inst in label.getLines()) {
                // Pattern match
                when (inst) {
                    is Sub -> {
                        val dest = regAllocMap.regAssignment[inst.dest]
                        val base = regAllocMap.regAssignment[inst.base]
                        val op = replaceOperandRegister(inst.op, regAllocMap)
                        instList.add(Sub(inst.conditionCode!!, dest!!, base!!, op))
                    }
                    is ReverseSub -> {
                        val dest = regAllocMap.regAssignment[inst.dest]
                        val base = regAllocMap.regAssignment[inst.base]
                        val op = replaceOperandRegister(inst.op, regAllocMap)
                        instList.add(ReverseSub(inst.conditionCode!!, dest!!, base!!, op))
                    }
                    is Mult -> {
                        val dest = regAllocMap.regAssignment[inst.dest]
                        val reg_n = regAllocMap.regAssignment[inst.reg_n]
                        val reg_m = regAllocMap.regAssignment[inst.reg_m]
                        instList.add(Mult(inst.conditionCode!!, dest!!, reg_n!!, reg_m!!))
                    }
                    is LongMult -> {
                        val dest_lo = regAllocMap.regAssignment[inst.dest_lo]
                        val dest_hi = regAllocMap.regAssignment[inst.dest_hi]
                        val reg_n = regAllocMap.regAssignment[inst.reg_n]
                        val reg_m = regAllocMap.regAssignment[inst.reg_m]
                        instList.add(LongMult(inst.conditionCode!!, dest_lo!!, dest_hi!!, reg_n!!, reg_m!!))
                    }
                    is Branch -> {
                        instList.add(inst)
                    }
                    is BranchLink -> {
                        instList.add(inst)
                    }
                    is Compare -> {
                        val base = regAllocMap.regAssignment[inst.base]!!
                        val op = replaceOperandRegister(inst.op, regAllocMap)
                        instList.add(Compare(inst.conditionCode, base, op))
                    }
                    is LoadWord -> {
                        val dest = regAllocMap.regAssignment[inst.dest]
                        val addr = replaceOperandRegister(inst.addr, regAllocMap)
                        instList.add(LoadWord(inst.conditionCode, dest!!, addr as AddressOperand))
                    }
                    is LoadByte -> {
                        val dest = regAllocMap.regAssignment[inst.dest]
                        val addr = replaceOperandRegister(inst.addr, regAllocMap)
                        instList.add(LoadByte(inst.conditionCode, dest!!, addr as AddressOperand))
                    }
                    is And -> {
                        val dest = regAllocMap.regAssignment[inst.dest]
                        val base = regAllocMap.regAssignment[inst.base]
                        val op = replaceOperandRegister(inst.op, regAllocMap)
                        instList.add(And(inst.conditionCode, inst.updateFlags, dest!!, base!!, op))
                    }
                    is Or -> {
                        val dest = regAllocMap.regAssignment[inst.dest]
                        val base = regAllocMap.regAssignment[inst.base]
                        val op = replaceOperandRegister(inst.op, regAllocMap)
                        instList.add(Or(inst.conditionCode, inst.updateFlags, dest!!, base!!, op))
                    }
                    is Xor -> {
                        val dest = regAllocMap.regAssignment[inst.dest]
                        val base = regAllocMap.regAssignment[inst.base]
                        val op = replaceOperandRegister(inst.op, regAllocMap)
                        instList.add(Xor(inst.conditionCode, inst.updateFlags, dest!!, base!!, op))
                    }
                    is Move -> {
                        val dest = regAllocMap.regAssignment[inst.dest]
                        val op = replaceOperandRegister(inst.op, regAllocMap)
                        instList.add(Move(inst.conditionCode,  dest!!, op, inst.updateFlags))
                    }
                    is MoveNot -> {
                        val dest = regAllocMap.regAssignment[inst.dest]
                        val op = replaceOperandRegister(inst.op, regAllocMap)
                        instList.add(MoveNot(inst.conditionCode,  dest!!, op, inst.updateFlags))
                    }
                    is Push -> {
                        val regs = replaceAllRegs(inst.registers.toList(), regAllocMap)
                        instList.add(Push(inst.conditionCode, *regs.toTypedArray()))
                    }
                    is Pop -> {
                        val regs = replaceAllRegs(inst.registers.toList(), regAllocMap)
                        instList.add(Pop(inst.conditionCode, *regs.toTypedArray()))
                    }
                    is StoreWord -> {
                        val src = regAllocMap.regAssignment[inst.src]
                        val addr = replaceOperandRegister(inst.addr, regAllocMap)
                        instList.add(StoreWord(inst.conditionCode, src!!, addr as AddressOperand))
                    }
                    is StoreByte -> {
                        val src = regAllocMap.regAssignment[inst.src]
                        val addr = replaceOperandRegister(inst.addr, regAllocMap)
                        instList.add(StoreByte(inst.conditionCode, src!!, addr as AddressOperand))
                    }
                }
            }

            blockToLabelMap[block] = newBranchLabel(label.name)
            blockToLabelMap[block]!!.addLines(instList)
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
            else -> {
            }
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
            else -> {
            }
        }
    }

    @TranslatorMethod
    private fun translateBranchIf(node: TacBranchIf) {
        // CMP resultReg, 1
        // BEQ loop
        // short circuiting
        val currBlock = currentLabel
        val label = blockToLabelMap.computeIfAbsent(node.block) {
            translateBasicBlock(it)
        }
        currentLabel = currBlock

        if (node.cond is BoolImm) {
            if (node.cond.value) {
                addLines(Branch(BranchLabelOperand(label)))
            }
            // otherwise dont add instruction at all
        } else {
            val condReg = translateVar(node.cond as TacVar)

            addLines(
                Compare(condReg, IntImmediateOperand(1)),
                Branch(EQ, BranchLabelOperand(label))
            )
        }
    }

    @TranslatorMethod
    private fun translateBranch(node: TacBranch) {
        val currBlock = currentLabel
        val label = blockToLabelMap.computeIfAbsent(node.block) {
            translateBasicBlock(it)
        }
        currentLabel = currBlock
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

    private fun replaceOperandRegister(op: Operand, regAllocMap: RegAllocResult): Operand {
        return when (op) {
            is RegisterList -> {
                val actualRegs = replaceAllRegs(op.registers, regAllocMap)
                RegisterList(*actualRegs.toTypedArray())
            }
            is LogicalShiftLeft -> {
                val base = regAllocMap.regAssignment[op.base]!!
                LogicalShiftLeft(base, op.bits)
            }
            is LogicalShiftRight -> {
                val base = regAllocMap.regAssignment[op.base]!!
                LogicalShiftRight(base, op.bits)
            }
            is ArithmeticShiftRight -> {
                val base = regAllocMap.regAssignment[op.base]!!
                ArithmeticShiftRight(base, op.bits)
            }
            is RotateRight -> {
                val base = regAllocMap.regAssignment[op.base]!!
                RotateRight(base, op.bits)
            }
            is ImmediateOffset -> {
                val base = regAllocMap.regAssignment[op.base]!!
                ImmediateOffset(base, op.offset)
            }
            is ZeroOffset -> {
                val base = regAllocMap.regAssignment[op.base]!!
                ZeroOffset(base)
            }
            is RegisterOffset -> {
                val base = regAllocMap.regAssignment[op.base]!!
                val offset = regAllocMap.regAssignment[op.offsetReg]!!
                RegisterOffset(base, op.positiveOffset, offset)
            }
            else -> op
        }
    }

    private fun replaceAllRegs(registers: List<Register>, regAllocMap: RegAllocResult): List<Register> {
        return registers.map{ r -> regAllocMap.regAssignment[r]!! }
    }
}
