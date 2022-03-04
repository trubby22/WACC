package ic.doc.group15.codegen.assembly.instruction

/**
 * Condition codes present in ARM1176JZF-S, partly implemented.
 */
enum class ConditionCode {
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
    LE,

    /**
     * Overflow.
     **/
    V,

    /**
     * Carry.
     **/
    C
    ;

    override fun toString(): String {
        return name.uppercase()
    }
}
