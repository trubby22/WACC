package ic.doc.group15.codegen.assembly

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

abstract class Instruction : Line()
