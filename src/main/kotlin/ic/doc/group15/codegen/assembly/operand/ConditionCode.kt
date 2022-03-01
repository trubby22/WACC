package ic.doc.group15.codegen.assembly.operand

/**
 * Condition codes present in ARM1176JZF-S, partly implemented.
 */
enum class ConditionCode : Operand {
    /**
     * Equal.
     **/
    EQ,

    /**
     * Not equal.
     **/
    NE,

    /**
     * Signed greater or equal.
     **/
    GE,

    /**
     * Signed less than.
     **/
    LT,

    /**
     * Signed greater than.
     **/
    GT,

    /**
     * Signed less than or equal.
     **/
    LE
    ;

    override fun toString(): String {
        return name.lowercase()
    }
}
