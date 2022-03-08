package ic.doc.group15.codegen.assembly.operand

import ic.doc.group15.codegen.assembly.Assembly
import ic.doc.group15.codegen.assembly.BranchLabel
import ic.doc.group15.codegen.assembly.DataLabel
import ic.doc.group15.codegen.assembly.Label
import ic.doc.group15.util.EscapeChar

/**
 * Operand2 adopted in ARM1176JZF-S. Register controlled shift is not implemented.
 */

interface Operand : Assembly

abstract class LabelOperand<T : Label<*>> protected constructor(
    val labelName: String,
    val prefix: String = ""
) : Operand {

    constructor(label: T, prefix: String = "") : this(label.name, prefix)

    override fun toString(): String {
        return prefix + labelName
    }
}

open class BranchLabelOperand : LabelOperand<BranchLabel> {
    constructor(label: BranchLabel) : super(label)
    constructor(labelName: String) : super(labelName)
}

class DataLabelOperand(label: DataLabel) : LabelOperand<DataLabel>(label, "="), AddressOperand

class RegisterList(vararg regs: Register) : Operand {

    val registers: List<Register>

    init {
        regs.sort()
        registers = regs.toList()
    }

    override fun toString(): String {
        return registers.joinToString(separator = ",", prefix = "{", postfix = "}")
    }
}

/**
 * This form uses an integer constant as the operand. Note that the sign value
 * of the offset can be inferred from the signed integer argument value.
 * The offset must be a signed integer of size 4 bytes, formed by right-rotating
 * an 8-bit value by an even number of bits. They can be either be in the form
 * 0x00XY00XY, 0xXY00XY00, or 0xXYXYXYXY.
 * Format: #<value>
 *
 * @param value
 * @exception IllegalArgumentException
 */
abstract class ImmediateOperand<T> protected constructor(val value: T) : Operand {
    init {
        // TODO(Check if value is valid by performing rotation checks)
    }

    override fun toString(): String {
        return "#$value"
    }
}

class IntImmediateOperand(value: Int) : ImmediateOperand<Int>(value)

class CharImmediateOperand(value: Char) : ImmediateOperand<Char>(value) {

    val string = EscapeChar.fromChar(value) ?: "$value"

    override fun toString(): String {
        return "#\'$string\'"
    }
}

class BoolImmediateOperand(value: Boolean) : ImmediateOperand<Boolean>(value) {

    override fun toString(): String {
        return "#${if (value) 1 else 0}"
    }
}

/**
 * This form uses an integer constant as the operand, and is only used for
 * the pseudo-instruction "LDR reg =imm" and not any other instructions. It
 * allows any immediate value to be loaded into a register, bypassing the
 * limitations of the MOV instruction.
 * Format: =<value>
 *
 * @param value
 */
class PseudoImmediateOperand(val value: Int) : Operand, AddressOperand {
    override fun toString(): String {
        return "=$value"
    }
}

abstract class ShiftOperand protected constructor(
    val opName: String,
    val base: Register,
    val bits: Int,
    val minBits: Int = 1,
    val maxBits: Int = 32
) : Operand {

    init {
        if (!IntRange(minBits, maxBits).contains(bits)) {
            throw IllegalArgumentException(
                "$opName must be between $minBits and $maxBits bits. Actual: $bits"
            )
        }
    }

    override fun toString(): String {
        return "$base, $opName #$bits"
    }
}

/**
 * This form performs a logical shift left of specified bits on the value stored
 * in the base register with an immediate constant. The bit count must be a
 * five-bit integer in range [1, 31].
 *
 * Note: The contents of the base register remain unchanged. Specifying a
 * register with shift also updates the carry flag when used with certain
 * instructions.
 *
 * Format: <base> LSL #<bits>
 *
 * @param base The base register
 * @param bits Shift bit-count
 * @exception IllegalArgumentException
 */
class LogicalShiftLeft(base: Register, bits: Int) : ShiftOperand("LSL", base, bits, maxBits = 31)

/**
 * This form performs a logical shift right of specified bits on the value stored
 * in the base register with an immediate constant. The bit count must be a
 * five-bit integer in range [1, 32].
 *
 * Note: The contents of the base register remain unchanged. Specifying a
 * register with shift also updates the carry flag when used with certain
 * instructions.
 *
 * Format: <base> LSR #<bits>
 *
 * @param base The base register
 * @param bits Shift bit-count
 * @exception IllegalArgumentException
 */
class LogicalShiftRight(base: Register, bits: Int) : ShiftOperand("LSR", base, bits)

/**
 * This form performs an arithmetic shift right of specified bits on the value stored
 * in the base register with an immediate constant. The bit count must be a
 * five-bit integer in range [1, 32].
 *
 * Note: The contents of the base register remain unchanged. Specifying a
 * register with shift also updates the carry flag when used with certain
 * instructions.
 *
 * Format: <base> LSL #<bits>
 *
 * @param base The base register
 * @param bits Shift bit-count
 * @exception IllegalArgumentException
 */
class ArithmeticShiftRight(base: Register, bits: Int) : ShiftOperand("ASR", base, bits)

/**
 * This form performs a rotate-right of specified bits on the value stored
 * in the base register with an immediate constant. The bit count must be a
 * five-bit integer in range [1, 31].
 *
 * Note: The contents of the base register remain unchanged. Specifying a
 * register with shift also updates the carry flag when used with certain
 * instructions.
 *
 * Format: <base> ROR #<bits>
 *
 * @param base The base register
 * @param bits Shift bit-count
 * @exception IllegalArgumentException
 */
class RotateRight(base: Register, bits: Int) : ShiftOperand("ROR", base, bits, maxBits = 31)
