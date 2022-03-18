package ic.doc.group15.assembly.operand

interface Register : Operand

/**
 * Registers present in ARM1176JZF-S.
 */
enum class ArmRegister : Register {

    /**
     * By convention, R0 is a general purpose register. It is used to
     * pass the first argument to a subroutine, and to pass the result
     * back to the caller.
     *
     * Note: a subroutine that requires more than
     * four inputs uses the stack for the additional inputs.
     */
    R0,

    /**
     * By convention, R1 is a general purpose register. It is used to
     * pass the second argument to a subroutine.
     *
     * Note: a subroutine that requires more than
     * four inputs uses the stack for the additional inputs.
     */
    R1,

    /**
     * By convention, R2 is a general purpose register. It is used to
     * pass the third argument to a subroutine.
     *
     * Note: a subroutine that requires more than
     * four inputs uses the stack for the additional inputs.
     */
    R2,

    /**
     * By convention, R3 is a general purpose register. It is used to
     * pass the fourth argument to a subroutine.
     *
     * Note: a subroutine that requires more than
     * four inputs uses the stack for the additional inputs.
     */
    R3,

    /**
     * By convention, R4 is a general purpose register.
     */
    R4,

    /**
     * By convention, R5 is a general purpose register.
     */
    R5,

    /**
     * By convention, R6 is a general purpose register.
     */
    R6,

    /**
     * By convention, R7 holds the syscall number.
     */
    R7,

    /**
     * By convention, R8 is a general purpose register.
     */
    R8,

    /**
     * By convention, R9 is a general purpose register.
     */
    R9,

    /**
     * By convention, R10 is a general purpose register.
     */
    R10,

    /**
     * By convention, R11 is a general purpose register.
     */
    R11,

    /**
     * By convention, IP (or R12) is the intra procedural call register.
     */
    IP,

    /**
     * By convention, SP (or R13) is the stack pointer.
     * Use of SP as a general purpose register is discouraged.
     * The instruction descriptions mention when SP and PC can be used.
     *
     * Note that, however, the use of the SP in an ARM instruction,
     * in any way that is not possible in the corresponding Thumb
     * instruction, is deprecated.
     */
    SP,

    /**
     * By convention, LR (or R14) is the link register.
     */
    LR,

    /**
     * By convention, PC (or R15) is the program counter.
     *
     * The PC can be used explicitly in some ARM data processing
     * instructions, and implicitly in branch instructions.
     *
     * The address of the currently executing instruction is
     * typically PC–8 for ARM.
     */
    PC,

    /**
     * By convention, CPSR (or R16) is the current program status register.
     * It stores the program status flags as follows:
     * - The APSR flags
     * - The processor mode
     * - The interrupt disable flags
     * - The instruction set state (ARM, etc)
     * - The endianness state
     * - The execution state bits for the IT block
     */
    CPSR,
    ;

    override fun toString(): String {
        return name.lowercase()
    }

    fun nextReg(): ArmRegister {
        if (this == MAX_REG) {
            throw InstantiationException("Max register limit reached!")
        }
        return values()[this.ordinal + 1]
    }

    fun prevReg(): ArmRegister {
        if (this == MIN_REG) {
            throw InstantiationException("Min register limit reached!")
        }
        return values()[this.ordinal - 1]
    }

    companion object {
        val MIN_REG = R0
        val MAX_REG = R11
    }
}
