package ic.doc.group15.codegen.assembly

import ic.doc.group15.codegen.assembly.LibraryFunction.Companion.EXIT
import ic.doc.group15.codegen.assembly.LibraryFunction.Companion.FFLUSH
import ic.doc.group15.codegen.assembly.LibraryFunction.Companion.FREE
import ic.doc.group15.codegen.assembly.LibraryFunction.Companion.PRINTF
import ic.doc.group15.codegen.assembly.LibraryFunction.Companion.PUTS
import ic.doc.group15.codegen.assembly.LibraryFunction.Companion.SCANF
import ic.doc.group15.codegen.assembly.instruction.* // ktlint-disable no-unused-imports
import ic.doc.group15.codegen.assembly.instruction.ConditionCode.*
import ic.doc.group15.codegen.assembly.operand.DataLabelOperand
import ic.doc.group15.codegen.assembly.operand.ImmediateOffset
import ic.doc.group15.codegen.assembly.operand.ImmediateOperand
import ic.doc.group15.codegen.assembly.operand.Register.*
import ic.doc.group15.codegen.assembly.operand.ZeroOffset
import java.util.*

private const val NULL = '\u0000'

enum class UtilFunction {
    P_READ_INT {
        override val assembly by lazy {
            listOf(
                Push(LR),
                Move(R1, R0),
                LoadWord(R0, generateStringData("%d")),
                Add(R0, R0, ImmediateOperand(4)),
                BranchLink(SCANF),
                Pop(PC)
            )
        }
    },
    P_READ_CHAR {
        override val assembly by lazy {
            listOf(
                Push(LR),
                Move(R1, R0),
                LoadWord(R0, generateStringData(" %c")),
                Add(R0, R0, ImmediateOperand(4)),
                BranchLink(SCANF),
                Pop(PC)
            )
        }
    },
    P_CHECK_DIVIDE_BY_ZERO {
        override val assembly by lazy {
            listOf(
                Push(LR),
                Compare(R1, ImmediateOperand(0)),
                LoadWord(
                    EQ, R0, generateStringData("DivideByZeroError: divide or modulo by zero\n")
                ),
                BranchLink(EQ, P_THROW_RUNTIME_ERROR),
                Pop(PC)
            )
        }
    },
    P_THROW_RUNTIME_ERROR {
        override val assembly by lazy {
            listOf(
                BranchLink(P_PRINT_STRING),
                Move(R0, ImmediateOperand(-1)),
                BranchLink(EXIT)
            )
        }
    },
    P_THROW_OVERFLOW_ERROR {
        override val assembly by lazy {
            listOf(
                LoadWord(
                    R0,
                    generateStringData(
                        "OverflowError: the result is" +
                            " too small/large to store in a 4-byte signed-integer.\n"
                    )
                ),
                BranchLink(P_THROW_RUNTIME_ERROR)
            )
        }
    },
    P_CHECK_ARRAY_BOUNDS {
        override val assembly by lazy {
            listOf(
                Push(LR),
                Compare(R0, ImmediateOperand(0)),
                LoadWord(
                    LT, R0,
                    generateStringData("ArrayIndexOutOfBoundsError: negative index\n")
                ),
                LoadWord(R1, ZeroOffset(R1)),
                Compare(R0, R1),
                LoadWord(
                    C, R0,
                    generateStringData("ArrayIndexOutOfBoundsError: index too large\n")
                ),
                BranchLink(C, P_THROW_RUNTIME_ERROR),
                Pop(PC)
            )
        }
    },
    P_CHECK_NULL_POINTER {
        override val assembly by lazy {
            listOf(
                Push(LR),
                Compare(R0, ImmediateOperand(0)),
                LoadWord(
                    EQ, R0,
                    generateStringData("NullReferenceError: dereference a null reference\n")
                ),
                BranchLink(EQ, P_THROW_RUNTIME_ERROR),
                Pop(PC)
            )
        }
    },
    P_PRINT_STRING {
        override val assembly by lazy {
            listOf(
                Push(LR),
                LoadWord(R1, ZeroOffset(R0)),
                Add(R2, R0, ImmediateOperand(4)),
                LoadWord(R0, generateStringData("%.*s")),
                Add(R0, R0, ImmediateOperand(4)),
                BranchLink(PRINTF),
                Move(R0, ImmediateOperand(0)),
                BranchLink(FFLUSH),
                Pop(PC)
            )
        }
    },
    P_PRINT_INT {
        override val assembly by lazy {
            listOf(
                Push(LR),
                Move(R1, R0),
                LoadWord(R0, generateStringData("%d")),
                Add(R0, R0, ImmediateOperand(4)),
                BranchLink(PRINTF),
                Move(R0, ImmediateOperand(0)),
                BranchLink(FFLUSH),
                Pop(PC)
            )
        }
    },
    P_PRINT_BOOL {
        override val assembly by lazy {
            listOf(
                Push(LR),
                Compare(R0, ImmediateOperand(0)),
                LoadWord(NE, R0, generateStringData("true")),
                LoadWord(EQ, R0, generateStringData("false")),
                Add(R0, R0, ImmediateOperand(4)),
                BranchLink(PRINTF),
                Move(R0, ImmediateOperand(0)),
                BranchLink(FFLUSH),
                Pop(PC)
            )
        }
    },
    P_PRINT_REFERENCE {
        override val assembly by lazy {
            listOf(
                Push(LR),
                Move(R1, R0),
                LoadWord(R0, generateStringData("%p")),
                Add(R0, R0, ImmediateOperand(4)),
                BranchLink(PRINTF),
                Move(R0, ImmediateOperand(0)),
                BranchLink(FFLUSH),
                Pop(PC)
            )
        }
    },
    P_PRINT_LN {
        override val assembly by lazy {
            listOf(
                Push(LR),
                LoadWord(R0, generateStringData("")),
                Add(R0, R0, ImmediateOperand(4)),
                BranchLink(PUTS),
                Move(R0, ImmediateOperand(0)),
                BranchLink(FFLUSH),
                Pop(PC)
            )
        }
    },
    P_FREE_PAIR {
        override val assembly by lazy {
            listOf(
                Push(LR),
                Compare(R0, ImmediateOperand(0)),
                LoadWord(
                    EQ, R0,
                    generateStringData("NullReferenceError: dereference a null reference\n")
                ),
                Branch(EQ, P_THROW_RUNTIME_ERROR),
                Push(R0),
                LoadWord(R0, ZeroOffset(R0)),
                BranchLink(FREE),
                LoadWord(R0, ZeroOffset(SP)),
                LoadWord(R0, ImmediateOffset(SP, 4)),
                BranchLink(FREE),
                Pop(R0),
                BranchLink(FREE),
                Pop(PC)
            )
        }
    }
    ;

    val labelName = name.lowercase()

    val dataBlocks: MutableList<DataLabel> = LinkedList()
    val labelBlock: BranchLabel by lazy { BranchLabel(name.lowercase(), assembly) }

    protected abstract val assembly: List<Instruction>

    private val stringLabel: UniqueLabelGenerator = UniqueLabelGenerator(labelName + "_msg_")

    protected fun generateStringData(str: String): DataLabelOperand {
        val label = StringData(stringLabel, str, nullTerminated = true)
        dataBlocks.add(label)
        return DataLabelOperand(label)
    }
}
