package ic.doc.group15.codegen.assembly

import ic.doc.group15.codegen.assembly.ArmFunction.* // ktlint-disable no-unused-imports
import java.util.*

const val NULL = "\u0000"

enum class UtilFunction {

    P_READ_INT {
        override fun assembly(): List<Instruction> {
            return listOf(
                PUSHlr(),
                MOVreg(1, 0),
                LDRimmString(0, generateStringData("%d")),
                ADD(0, 0, 5),
                BL(SCANF.label()),
                POPpc()
            )
        }
    }
    ;

    val labelName = name.lowercase()

    val dataBlocks: MutableList<Data> = LinkedList()
    val labelBlock: BranchLabel

    protected val stringLabel: UniqueLabel = UniqueLabel(labelName + "_msg_")

    init {
        labelBlock = BranchLabel(name.lowercase(), assembly())
    }

    protected abstract fun assembly(): List<Instruction>

    protected fun generateStringData(str: String): String {
        val label = stringLabel.generate()
        dataBlocks.add(StringData(label, str))
        return label
    }
}
