package ic.doc.group15.instructions

/**
 * Operand2 adopted in ARM1176JZF-S. Register controlled shift is not implemented.
 */
abstract class Operand2: Translatable()

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
class ImmOp(val value: Int): Operand2() {
  init {
    // TODO(Check if value is valid by performing rotation checks)
  }

  override fun translate(): String {
    return "#$value"
  }
}

/**
 * This form uses the base register as the operand.
 * Format: <base>
 *
 * @param base The base register
 */
class RegOp(val base: Register): Operand2() {
  override fun translate(): String {
    return "$base"
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
class RegLSLImmOp(val base: Register, val bits: Int): Operand2() {
  init {
    if (!IntRange(1, 31).contains(bits)) {
      throw IllegalArgumentException("Operand2 can only support logical shift left between 1 and 31 bits! The bit count passed in is $bits instead")
    }
  }

  override fun translate(): String {
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
class RegLSRImmOp(val base: Register, val bits: Int): Operand2() {
  init {
    if (!IntRange(1, 32).contains(bits)) {
      throw IllegalArgumentException("Operand2 can only support logical shift right between 1 and 32 bits! The bit count passed in is $bits instead")
    }
  }

  override fun translate(): String {
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
class RegASRImmOp(val base: Register, val bits: Int): Operand2() {
  init {
    if (!IntRange(1, 32).contains(bits)) {
      throw IllegalArgumentException("Operand2 can only support arithmetic shift right between 1 and 32 bits! The bit count passed in is $bits instead")
    }
  }

  override fun translate(): String {
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
class RegRORImmOp(val base: Register, val bits: Int): Operand2() {
  init {
    if (!IntRange(1, 31).contains(bits)) {
      throw IllegalArgumentException("Operand2 can only support logical shift left between 1 and 31 bits! The bit count passed in is $bits instead")
    }
  }

  override fun translate(): String {
    return "[$base ROR #$bits]"
  }
}
