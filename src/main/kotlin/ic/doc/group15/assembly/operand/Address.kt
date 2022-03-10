package ic.doc.group15.assembly.operand

/**
 * Addressing mode 2 adopted in ARM1176JZF-S. Pre-indexed offset and post-indexed offset are
 * not implemented.
 */
interface AddressOperand : Operand

/**
 * This form uses a constant as an offset. Note that the sign value of the
 * offset can be inferred from the signed integer argument value.
 * The offset must be a signed integer of size 12 bits (ie in range [-2048, 2047])
 * Format: [<base>, #+/-<offset>]
 *
 * @param base The base register
 * @param offset The offset value
 * @exception IllegalArgumentException
 */
class ImmediateOffset(val base: Register, val offset: Int) : AddressOperand {
    init {
        if (!IntRange(-2048, 2047).contains(offset)) {
            throw IllegalArgumentException(
                "Value of offset: $offset exceeds range of an 12-bit constant"
            )
        }
    }

    override fun toString(): String {
        return "[$base, #$offset]"
    }
}

/**
 * This form takes the value of base register as the address.
 * Format: [<base>]
 *
 * @param base The base register
 */
class ZeroOffset(val base: Register) : AddressOperand {
    override fun toString(): String {
        return "[$base]"
    }
}

/**
 * This form uses a register as an offset.
 * Format: [<base>, #+/-<offsetReg>]
 *
 * @param base The base register
 * @param positiveOffset Boolean value that decides if offset should be positive or negative
 * @param offsetReg The register containing the offset value
 */
class RegisterOffset(
    val base: Register,
    val positiveOffset: Boolean,
    val offsetReg: Register
) : AddressOperand {
    override fun toString(): String {
        return "[$base, #${if (!positiveOffset) { "-" } else { "" }}$offsetReg]"
    }
}
