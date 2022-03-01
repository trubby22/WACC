package ic.doc.group15.instructions

abstract class Translatable {
  abstract fun translate(): String

  override fun toString(): String {
    return translate()
  }
}