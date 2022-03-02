package ic.doc.group15.codegen.assembly

import ic.doc.group15.codegen.assembly.ArmFunction.* // ktlint-disable no-unused-imports
import ic.doc.group15.codegen.assembly.ArmFunction.Companion.SCANF
import ic.doc.group15.codegen.assembly.instruction.*
import ic.doc.group15.codegen.assembly.operand.DataLabelOperand
import ic.doc.group15.codegen.assembly.operand.ImmediateOperand
import ic.doc.group15.codegen.assembly.operand.Register.*
import java.util.*

const val NULL = "\u0000"

enum class UtilFunction {

    P_READ_INT {
        override val assembly = listOf(
            Push(LR),
            Move(R1, R0),
            LoadWord(R0, generateStringData("%d")),
            Add(R0, R0, ImmediateOperand(4)),
            BranchLink(SCANF.labelName),
            Pop(PC)
        )
    },
    P_CHECK_DIVIDE_BY_ZERO {
        override val assembly = listOf(
            Push(LR),
            Compare(R1, ImmediateOperand(0)),
//            The corresponding message should be "DivideByZeroError: divide or modulo by zero\n\0"
            LoadWord(conditionCode = ConditionCode.EQ, R0, DataLabelOperand
                ("TODO")),
            BranchLink(ConditionCode.EQ, "p_throw_runtime_error"),
            Pop(PC)
        )
    },
    P_THROW_RUNTIME_ERROR {
        override val assembly= listOf(
            BranchLink("p_print_string"),
            Move(R0, ImmediateOperand(-1)),
            BranchLink("exit")
        )
    },
    P_PRINT_STRING {
        override val assembly: List<Instruction> = listOf(
            Push(LR),
            LoadWord(R1, R0),
            Add(R2, R0, ImmediateOperand(4)),
//            Corresponding message in .data sector: "%.*s\0"
            LoadWord(R0, DataLabelOperand("TODO")),
            Add(R0, R0, ImmediateOperand(4)),
            BranchLink("printf"),
            Move(R0, ImmediateOperand(0)),
            BranchLink("fflush"),
            Pop(PC)
        )
    }
    ;

    val labelName = name.lowercase()

    val dataBlocks: MutableList<Data> = LinkedList()
    val labelBlock: BranchLabel

    abstract val assembly: List<Instruction>

    protected val stringLabel: UniqueLabel = UniqueLabel(labelName + "_msg_")

    init {
        labelBlock = BranchLabel(name.lowercase(), assembly)
    }

    protected fun generateStringData(str: String): DataLabelOperand {
        val label = stringLabel.generate()
        dataBlocks.add(StringData(label, str))
        return DataLabelOperand(label)
    }
}
