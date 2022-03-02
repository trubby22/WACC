package ic.doc.group15.codegen.assembly

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
    }
    ;

    val labelName = name.lowercase()

    val dataBlocks: MutableList<Data> = LinkedList()
    val labelBlock: BranchLabel

    abstract val assembly: List<Instruction>

    protected val stringLabel: UniqueLabelGenerator = UniqueLabelGenerator(labelName + "_msg_")

    init {
        labelBlock = BranchLabel(name.lowercase(), assembly)
    }

    protected fun generateStringData(str: String): DataLabelOperand {
        val label = stringLabel.generate()
        dataBlocks.add(StringData(label, str))
        return DataLabelOperand(label)
    }
}
