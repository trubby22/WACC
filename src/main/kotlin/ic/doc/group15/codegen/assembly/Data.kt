package ic.doc.group15.codegen.assembly

abstract class DataLine : Line()

abstract class Data protected constructor(
    labelName: String,
    val byteSize: Int,
    vararg lines: DataLine
) : Label<DataLine>(labelName, Word(byteSize), *lines)

class StringData(name: String, val str: String) : Data(name, str.length + 1, Ascii(str))

private class Word(val byteSize: Int) : DataLine() {
    override fun toString(): String {
        return ".word $byteSize"
    }
}

private class Ascii(val str: String) : DataLine() {
    override fun toString(): String {
        return ".ascii".padEnd(ASM_TAB_SIZE) + "\"str\""
    }
}
