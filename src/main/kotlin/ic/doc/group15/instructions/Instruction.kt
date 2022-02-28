package ic.doc.group15.instructions

abstract class Instruction {
    abstract fun translate(): String

    override fun toString(): String {
        return translate()
    }
}

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

// TODO: Incorporate condition codes in instructions if required

/**
 * LDR is a load register instruction that loads the particular value
 * of word size (4 bytes) at a particular address into the destination register.
 *
 * @param dest The destination register
 * @param addr The source address
 */
class LDR(val dest: Register, val addr: AddressMode): Instruction() {
    override fun translate(): String {
        return "ldr $dest, $addr"
    }
}

/**
 * LDRB is a load register instruction that loads the particular value
 * of byte size (1 byte) at a particular address into the destination register.
 * This instruction is mainly used to load a character.
 *
 * @param dest The destination register
 * @param addr The source address
 */
class LDRB(val dest: Register, val addr: AddressMode): Instruction() {
    override fun translate(): String {
        return "ldrb $dest, $addr"
    }
}

/**
 * LDRSB is a load register instruction that loads the particular value
 * of signed byte size (1 byte) at a particular address into the destination register.
 * This instruction is mainly used to load a boolean.
 *
 * @param dest The destination register
 * @param addr The source address
 */
class LDRSB(val dest: Register, val addr: AddressMode): Instruction() {
    override fun translate(): String {
        return "ldrsb $dest, $addr"
    }
}

/**
 * STR is a store register instruction that stores the particular value
 * of word size (4 bytes) at the register into memory at the specified
 * address.
 *
 * @param src The source register
 * @param addr The destination address
 */
class STR(val src: Register, val addr: AddressMode): Instruction() {
    override fun translate(): String {
        return "str $src, $addr"
    }
}

/**
 * STRB is a store register instruction that stores the particular value
 * of byte size (1 byte) at the register into memory at the specified
 * address.
 *
 * @param src The source register
 * @param addr The destination address
 */
class STRB(val src: Register, val addr: AddressMode): Instruction() {
    override fun translate(): String {
        return "strb $src, $addr"
    }
}

/**
 * ADD is an add-without-carry instruction that adds the values in
 * the base register and operand, and stores the result in the
 * destination register.
 *
 * @param dest The destination register
 * @param base The base register/register holding the first operand
 * @param op A flexible second operand
 */
class ADD(val dest: Register, val base: Register, val op: Operand): Instruction() {
    override fun translate(): String {
        return "add $dest, $src, $op"
    }
}


/**
 * ADD is an add-without-carry instruction that adds the values in
 * the base register and operand, and stores the result in the
 * destination register. Since the S suffix is specified, the condition flags
 * N,Z,C,V are updated on the result of the operation.
 *
 * @param dest The destination register
 * @param base The base register/register holding the first operand
 * @param op A flexible second operand
 */
class ADDS(val dest: Register, val base: Register, val op: Operand): Instruction() {
    override fun translate(): String {
        return "adds $dest, $src, $op"
    }
}

/**
 * SUB is a subtract-without-carry instruction that subtracts the value of
 * operand from the value in the base register, and stores the result in
 * the destination register.
 *
 * @param dest The destination register
 * @param base The source register/register holding the first operand
 * @param op A flexible second operand
 */
class SUB(val dest: Register, val base: Register, val op: Operand): Instruction() {
    override fun translate(): String {
        return "sub $dest, $src, $op"
    }
}

/**
 * SUB is a subtract-without-carry instruction that subtracts the value of
 * operand from the value in the base register, and stores the result in
 * the destination register. Since the S suffix is specified, the condition flags
 * N,Z,C,V are updated on the result of the operation.
 *
 * Note: "SUBS pc, lr #imm" is a special case of the instruction - it performs
 * exception return without popping anything from the stack. In other words,
 * it subtracts the value from the link register and loads the PC with the
 * result, then copies the SPSR to the CPSR. "SUBS pc, lr #imm" can be used
 * to return from an exception if there is no return state on the stack. The
 * value of #imm depends on the exception to return from.
 *
 * @param dest The destination register
 * @param base The source register/register holding the first operand
 * @param op A flexible second operand
 */
class SUBS(val dest: Register, val base: Register, val op: Operand): Instruction() {
    override fun translate(): String {
        return "subs $dest, $src, $op"
    }
}

/**
 * CMP is a compare instruction that compares the value in the register with the
 * operand by subtracting the value of operand from the value in the register,
 * then update the condition flags N,Z,C,V on the result, but do not place the
 * result in any register. It is similar to a call to the SUBS instruction except
 * that the result is discarded.
 *
 * @param base The base register/register holding the first operand
 * @param op A flexible second operand
 */
class CMP(val base: Register, val op: Operand): Instruction() {
    override fun translate(): String {
        return "cmp $base $op"
    }
}