package ic.doc.group15.instructions

/**
 * Addressing mode 2 adopted in ARM1176JZF-S. Pre-indexed offset and
 * post-indexed offset are not implemented.
 */
abstract class AddressMode2: Translatable()

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
class ImmOffset2(val base: Register, val offset: Int): AddressMode2() {
  init {
    if (!IntRange(-2048, 2047).contains(offset)) {
      throw IllegalArgumentException("Value of offset: $offset exceeds range of an 12-bit constant")
    }
  }

  override fun translate(): String {
    return "[$base, #$offset]"
  }
}

/**
 * This form takes the value of base register as the address.
 * Format: [<base>]
 *
 * @param base The base register
 */
class ZeroOffset2(val base: Register): AddressMode2() {
  override fun translate(): String {
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
class RegOffset2(val base: Register, val positiveOffset: Boolean, val offsetReg: Register): AddressMode2() {
  override fun translate(): String {
    return "[$base, #${if (!positiveOffset) {"-"} else {""} }$offsetReg]"
  }
}

/**
 * Addressing mode 3 adopted in ARM1176JZF-S. Pre-indexed offset and
 * post-indexed offset are not implemented.
 */
abstract class AddressMode3 {
  abstract fun translate(): String

  override fun toString(): String {
    return translate()
  }
}

/**
 * This form uses a constant as an offset. Note that the sign value of the
 * offset can be inferred from the signed integer argument value.
 * The offset must be a signed integer of size 1 byte (ie in range [-128, 127])
 * Format: [<base>, #+/-<offset>]
 *
 * @param base The base register
 * @param offset The offset value
 * @exception IllegalArgumentException
 */
class ImmOffset3(val base: Register, val offset: Int): AddressMode3() {
  init {
    if (!IntRange(-128, 127).contains(offset)) {
      throw IllegalArgumentException("Value of offset: $offset exceeds range of an 8-bit constant")
    }
  }

  override fun translate(): String {
    return "[$base, #$offset]"
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
class RegOffset3(val base: Register, val positiveOffset: Boolean, val offsetReg: Register): AddressMode3() {
  override fun translate(): String {
    return "[$base, #${if (!positiveOffset) {"-"} else {""} }$offsetReg]"
  }
}
