package ic.doc.group15.codegen.assembly

import java.util.*

abstract class Label<L : Line> protected constructor(val name: String) : Assembly {

    private val lines: MutableList<L> = LinkedList()

    protected constructor(name: String, lines: List<L>) : this(name) {
        this.lines.addAll(lines)
    }

    protected constructor(name: String, vararg lines: L) : this(name) {
        this.lines.addAll(lines)
    }

    protected constructor(uniqueLabel: UniqueLabelGenerator, vararg lines: L) :
        this(uniqueLabel.generate(), *lines)

    fun addLines(vararg lines: L) {
        this.lines.addAll(lines)
    }

    fun addLines(lines: Collection<L>) {
        this.lines.addAll(lines)
    }

    override fun toString(): String {
        return "$name:\n$ASM_TAB" + lines.joinToString(separator = "\n$ASM_TAB")
    }
}

class BranchLabel : Label<Instruction> {
    constructor(name: String, lines: List<Instruction> = emptyList()) : super(name, lines)
    constructor(name: String, vararg lines: Instruction) : super(name, *lines)
    constructor(uniqueLabel: UniqueLabelGenerator, vararg lines: Instruction) : super(uniqueLabel, *lines)
}

abstract class DataLine : Line()

abstract class DataLabel protected constructor(
    labelName: String,
    val byteSize: Int,
    vararg lines: DataLine
) : Label<DataLine>(labelName, Word(byteSize), *lines) {

    protected constructor(uniqueLabel: UniqueLabelGenerator, byteSize: Int, vararg lines: DataLine) :
        this(uniqueLabel.generate(), byteSize, *lines)
}

class StringData(
    name: String,
    val str: String,
    nullTerminated: Boolean = false
) : DataLabel(name, str.length, Ascii(str, nullTerminated)) {

    constructor(uniqueLabel: UniqueLabelGenerator, str: String, nullTerminated: Boolean = false) :
        this(uniqueLabel.generate(), str, nullTerminated)
}

private class Word(val byteSize: Int) : DataLine() {
    override fun toString(): String {
        return ".word $byteSize"
    }
}

private class Ascii(val str: String, private val nullTerminated: Boolean = false) : DataLine() {
    override fun toString(): String {
        return ".ascii".padEnd(ASM_TAB_SIZE) + convertStr(str)
    }

    private fun convertStr(str: String): String {
        return "\"" + str.map {
            when (it) {
                '\b' -> "\\b"
                '\t' -> "\\t"
                '\n' -> "\\n"
                '\u000c' -> "\\f"
                '\r' -> "\\r"
                '\"' -> "\\\""
                '\'' -> "\\\'"
                '\\' -> "\\\\"
                else -> "$it"
            }
        }.joinToString(separator = "") + "${if (nullTerminated) "\\0" else ""}\""
    }
}
