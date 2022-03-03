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
        return ".$name:" + if (lines.isNotEmpty()) "\n$ASM_TAB" else "" + lines.joinToString(
            separator = "\n$ASM_TAB",
        )
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

class StringData(name: String, val str: String) : DataLabel(name, str.length + 1, Ascii(str)) {

    constructor(uniqueLabel: UniqueLabelGenerator, str: String) : this(uniqueLabel.generate(), str)
}

private class Word(val byteSize: Int) : DataLine() {
    override fun toString(): String {
        return ".word $byteSize"
    }
}

private class Ascii(val str: String) : DataLine() {
    override fun toString(): String {
        return ".ascii".padEnd(ASM_TAB_SIZE) + str
    }
}
