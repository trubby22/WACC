package ic.doc.group15.instructions

abstract class Instruction: Translatable()

/**
 * A label is a symbol that represents the memory address of an
 * instruction or data. The address can be PC-relative, register-relative,
 * or absolute. Labels are local to the source file unless you make them
 * global using the EXPORT directive.
 *
 * The address given by a label is calculated during assembly. armasm
 * calculates the address of a label relative to the origin of the section
 * where the label is defined. A reference to a label within the same section
 * can use the PC plus or minus an offset. This is called PC-relative addressing.
 * Addresses of labels in other sections are calculated at link time,
 * when the linker has allocated specific locations in memory for each section.
 *
 * @param value The string value of the label
 */
class Label(val value: String) : Instruction() {
    override fun translate(): String {
        return "$value:"
    }
}
