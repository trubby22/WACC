package ic.doc.group15.codegen.assembly

import ic.doc.group15.codegen.assembly.instruction.ConditionCode
import ic.doc.group15.codegen.assembly.operand.Operand
import java.util.*

const val ASM_TAB_SIZE = 8
val ASM_TAB = " ".repeat(ASM_TAB_SIZE)

interface Assembly

abstract class Line : Assembly

abstract class Label<L : Line> protected constructor(val name: String) : Assembly {

    private val lines: MutableList<L> = LinkedList()

    protected constructor(name: String, lines: List<L>) : this(name) {
        this.lines.addAll(lines)
    }

    protected constructor(name: String, vararg lines: L) : this(name) {
        this.lines.addAll(lines)
    }

    fun addLine(line: L) {
        addLines(line)
    }

    fun addLines(vararg lines: L) {
        this.lines.addAll(lines)
    }

    fun addLines(lines: Collection<L>) {
        this.lines.addAll(lines)
    }

    override fun toString(): String {
        return ".$name:\n" + lines.joinToString(separator = "\n$ASM_TAB")
    }
}

class BranchLabel : Label<Instruction> {
    constructor(name: String, lines: List<Instruction>) : super(name, lines)
    constructor(name: String, vararg lines: Instruction) : super(name, *lines)
    constructor(name: String) : super(name)
}

abstract class Instruction protected constructor(
    val instr: String,
    val conditionCode: ConditionCode? = null,
    protected vararg val params: Operand
) : Line() {
    override fun toString(): String {
        return "$instr${conditionCode ?: ""} " + params.joinToString(separator = ", ")
    }
}
