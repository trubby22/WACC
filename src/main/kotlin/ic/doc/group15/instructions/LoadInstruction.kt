package ic.doc.group15.instructions

/**
 * Load operations present in ARM1176JZF-S, partly implemented.
 */
abstract class LoadInstruction: Instruction()

/**
 * LDR is a load register instruction that loads the particular value
 * of word size (4 bytes) at a particular address into the destination register.
 *
 * @param dest The destination register
 * @param addr The source address
 */
class LDR(val dest: Register, val addr: AddressMode2): LoadInstruction() {
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
class LDRB(val dest: Register, val addr: AddressMode2): LoadInstruction() {
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
class LDRSB(val dest: Register, val addr: AddressMode3): LoadInstruction() {
  override fun translate(): String {
    return "ldrsb $dest, $addr"
  }
}