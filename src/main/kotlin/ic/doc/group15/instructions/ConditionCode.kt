package ic.doc.group15.instructions

/**
 * Condition codes present in ARM1176JZF-S, partly implemented.
 */
enum class ConditionCode {
  /**
   * Equal.
   **/
  EQ {
    override fun toString(): String { return "eq" }
  },

  /**
   * Not equal.
   **/
  NE {
    override fun toString(): String { return "ne" }
  },

  /**
   * Signed greater or equal.
   **/
  GE {
    override fun toString(): String { return "ge" }
  },

  /**
   * Signed less than.
   **/
  LT {
    override fun toString(): String { return "lt" }
  },

  /**
   * Signed greater than.
   **/
  GT {
    override fun toString(): String { return "gt" }
  },

  /**
   * Signed less than or equal.
   **/
  LE {
    override fun toString(): String { return "le" }
  }
}