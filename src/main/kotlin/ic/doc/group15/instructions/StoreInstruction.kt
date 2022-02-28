package ic.doc.group15.instructions

abstract class StoreInstruction: Instruction()

/**
 * STR is a store register instruction that stores the particular value
 * of word size (4 bytes) at the register into memory at the specified
 * address.
 *
 * @param src The source register
 * @param addr The destination address
 */
class STR(val src: Register, val addr: AddressMode2): StoreInstruction() {
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
class STRB(val src: Register, val addr: AddressMode2): StoreInstruction() {
  override fun translate(): String {
    return "strb $src, $addr"
  }
}

