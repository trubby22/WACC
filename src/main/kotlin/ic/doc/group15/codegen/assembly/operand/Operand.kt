package ic.doc.group15.codegen.assembly.operand

import ic.doc.group15.codegen.assembly.Assembly

/**
 * Operand2 adopted in ARM1176JZF-S. Register controlled shift is not implemented.
 */

interface Operand : Assembly

open class LabelOperand(val labelName: String) : Operand {
    override fun toString(): String {
        return labelName
    }
}

class DataLabelOperand(val labelName: String) : AddressOperand

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
class ImmediateOperand(val value: Int) : Operand {
    init {
        // TODO(Check if value is valid by performing rotation checks)
    }

    constructor(value: Boolean) : this(if (value) 1 else 0)

    constructor(value: Char) : this(value.code.toByte().toInt())

    override fun toString(): String {
        return "#$value"
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
class LogicalShiftLeft(val base: Register, val bits: Int) : Operand {
    init {
        if (!IntRange(1, 31).contains(bits)) {
            throw IllegalArgumentException(
                "Logical shift left must be between 1 and 31 bits. Actual: $bits"
            )
        }
    }

    override fun toString(): String {
        return "[$base LSL #$bits]"
    }
}

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
class LogicalShiftRight(val base: Register, val bits: Int) : Operand {
    init {
        if (!IntRange(1, 32).contains(bits)) {
            throw IllegalArgumentException(
                "Logical shift right must be between 1 and 32 bits. Actual: $bits"
            )
        }
    }

    override fun toString(): String {
        return "[$base LSR #$bits]"
    }
}

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
class ArithmeticShiftRight(val base: Register, val bits: Int) : Operand {
    init {
        if (!IntRange(1, 32).contains(bits)) {
            throw IllegalArgumentException(
                "Arithmetic shift right must between 1 and 32 bits. Actual: $bits"
            )
        }
    }

    override fun toString(): String {
        return "[$base ASR #$bits]"
    }
}

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
class RotateRight(val base: Register, val bits: Int) : Operand {
    init {
        if (!IntRange(1, 31).contains(bits)) {
            throw IllegalArgumentException(
                "Logical shift left between 1 and 31 bits. Actual: $bits"
            )
        }
    }

    override fun toString(): String {
        return "[$base ROR #$bits]"
    }
}
