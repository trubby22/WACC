package ic.doc.group15.ssa.optimisations

import ic.doc.group15.ast.BinaryOp
import ic.doc.group15.ssa.tac.*
import ic.doc.group15.type.BasicType
import java.util.*
import kotlin.math.ln
import kotlin.math.roundToInt

/**
 * Applies optimisation techniques local to each basic block in three address code form.
 */

/**
 * Simplify statements that are arithmetic/boolean identities.
 */
class OperationIdentity {
    companion object {
        fun apply(instruction: ThreeAddressCode): ThreeAddressCode {
            // Unary functions
            if (instruction is TacAssignCall) {
                return when (instruction.f) {
                    Functions.BANG -> checkNotIdentity(instruction)
                    else -> instruction
                }
            }

            // Skip if it is not a binary operation
            if (instruction !is TacAssignBinOp) return instruction

            val (_, op, _, _) = instruction

            return when (op) {
                BinaryOp.PLUS -> checkAdditionIdentity(instruction)
                BinaryOp.MINUS -> checkSubtractionIdentity(instruction)
                BinaryOp.MULT -> checkMultiplicationIdentity(instruction)
                BinaryOp.DIV -> checkDivisionIdentity(instruction)
                BinaryOp.EQUALS -> checkEqualsIdentity(instruction)
                BinaryOp.NOT_EQUALS -> checkNotEqualsIdentity(instruction)
                BinaryOp.AND -> checkAndIdentity(instruction)
                BinaryOp.OR -> checkOrIdentity(instruction)
                else -> instruction
            }
        }

        private fun checkAdditionIdentity(instruction: TacAssignBinOp): ThreeAddressCode {
            val (v, _, lhs, rhs) = instruction

            // v = 0 + a => v = a
            if (lhs is IntImm && lhs.value == 0) return TacAssignValue(v, rhs)

            // v = a + 0 => v = a
            if (rhs is IntImm && rhs.value == 0) return TacAssignValue(v, lhs)

            return instruction
        }

        private fun checkSubtractionIdentity(instruction: TacAssignBinOp): ThreeAddressCode {
            val (v, _, lhs, rhs) = instruction

            // v = a - 0 => v = a
            if (rhs is IntImm && rhs.value == 0) return TacAssignValue(v, lhs)

            // v = 0 - a => v = -a
            if (lhs is IntImm && rhs is IntImm && lhs.value == 0) return TacAssignValue(v, IntImm(-rhs.value))

            // v = a - a => v = 0
            if (lhs is TacVar && rhs is TacVar && lhs.id == rhs.id) return TacAssignValue(v, IntImm(0))

            return instruction
        }

        private fun checkMultiplicationIdentity(instruction: TacAssignBinOp): ThreeAddressCode {
            val (v, _, lhs, rhs) = instruction

            // v = 1 * a => v = a
            if (lhs is IntImm) {
                // v = 1 * a => v = a
                if (lhs.value == 1) return TacAssignValue(v, rhs)

                // v = 0 * a => v = 0
                if (lhs.value == 0) return TacAssignValue(v, IntImm(0))

                // v = -1 * a => v = -a
                if (rhs is IntImm && lhs.value == -1) return TacAssignValue(v, IntImm(-rhs.value))
            }

            if (rhs is IntImm) {
                // v = a * 1 => v = a
                if (rhs.value == 1) return TacAssignValue(v, lhs)

                // v = a * 0 => v = 0
                if (rhs.value == 0) return TacAssignValue(v, IntImm(0))

                // v = a * -1 => v = -a
                if (lhs is IntImm && rhs.value == -1) return TacAssignValue(v, IntImm(-lhs.value))
            }

            return instruction
        }

        private fun checkDivisionIdentity(instruction: TacAssignBinOp): ThreeAddressCode {
            val (v, _, lhs, rhs) = instruction

            if (rhs is IntImm) {
                // v = a / 1 => v = a
                if (rhs.value == 1) return TacAssignValue(v, lhs)

                // v = a / -1 => v = -a
                if (lhs is IntImm && rhs.value == -1) return TacAssignValue(v, IntImm(-lhs.value))
            }

            return instruction
        }

        private fun checkEqualsIdentity(instruction: TacAssignBinOp): ThreeAddressCode {
            val (v, _, lhs, rhs) = instruction

            if (lhs is IntImm && rhs is IntImm) {
                return TacAssignValue(v, BoolImm(lhs.value == rhs.value))
            }

            if (lhs is CharImm && rhs is CharImm) {
                return TacAssignValue(v, BoolImm(lhs.value == rhs.value))
            }

            if (lhs is BoolImm && rhs is BoolImm) {
                return TacAssignValue(v, BoolImm(lhs.value == rhs.value))
            }

            if (lhs is StrImm && rhs is StrImm) {
                return TacAssignValue(v, BoolImm(lhs.value == rhs.value))
            }

            if (lhs is TacVar && rhs is TacVar) {
                return TacAssignValue(v, BoolImm(lhs.id == rhs.id))
            }

            return instruction
        }

        private fun checkNotEqualsIdentity(instruction: TacAssignBinOp): ThreeAddressCode {
            val (v, _, lhs, rhs) = instruction

            if (lhs is IntImm && rhs is IntImm) {
                return TacAssignValue(v, BoolImm(lhs.value != rhs.value))
            }

            if (lhs is CharImm && rhs is CharImm) {
                return TacAssignValue(v, BoolImm(lhs.value != rhs.value))
            }

            if (lhs is BoolImm && rhs is BoolImm) {
                return TacAssignValue(v, BoolImm(lhs.value != rhs.value))
            }

            if (lhs is StrImm && rhs is StrImm) {
                return TacAssignValue(v, BoolImm(lhs.value == rhs.value))
            }

            if (lhs is TacVar && rhs is TacVar) {
                return TacAssignValue(v, BoolImm(lhs.id != rhs.id))
            }

            return instruction
        }

        private fun checkAndIdentity(instruction: TacAssignBinOp): ThreeAddressCode {
            val (v, _, lhs, rhs) = instruction

            // v = (true && a) => v = a
            if (lhs is BoolImm && lhs.value) return TacAssignValue(v, rhs)

            // v = (false && a) => v = false
            if (lhs is BoolImm && !lhs.value) return TacAssignValue(v, BoolImm(false))

            // v = (a && true) => v = a
            if (rhs is BoolImm && rhs.value) return TacAssignValue(v, rhs)

            // v = (a && false) => v = false
            if (rhs is BoolImm && !rhs.value) return TacAssignValue(v, BoolImm(false))

            // v = (a && a) => v = a
            if (lhs is TacVar && rhs is TacVar && lhs.id == rhs.id) return TacAssignValue(v, lhs)

            return instruction
        }

        private fun checkOrIdentity(instruction: TacAssignBinOp): ThreeAddressCode {
            val (v, _, lhs, rhs) = instruction

            // v = (true || a) => v = true
            if (lhs is BoolImm && lhs.value) return TacAssignValue(v, BoolImm(true))

            // v = (false || a) => v = a
            if (lhs is BoolImm && !lhs.value) return TacAssignValue(v, rhs)

            // v = (a || true) => v = true
            if (rhs is BoolImm && rhs.value) return TacAssignValue(v, BoolImm(true))

            // v = (a || false) => v = a
            if (rhs is BoolImm && !rhs.value) return TacAssignValue(v, lhs)

            // v = (a || a) => v = a
            if (lhs is TacVar && rhs is TacVar && lhs.id == rhs.id) return TacAssignValue(v, lhs)

            return instruction
        }

        private fun checkNotIdentity(instruction: TacAssignCall): ThreeAddressCode {
            assert(instruction.args.size == 1)
            val arg = instruction.args[0]

            if (arg is BoolImm) {
                return TacAssignValue(instruction.dest, BoolImm(!arg.value))
            }

            return instruction
        }
    }
}

/**
 * If both operands are of the same type and the immediate value is encoded, we can replace the
 * operation with the result of the computation.
 */
class ConstantFolding {
    companion object {
        fun apply(instruction: ThreeAddressCode): ThreeAddressCode {
            // Unary functions
            if (instruction is TacAssignCall) {
                return when (instruction.f) {
                    Functions.CHR -> foldChr(instruction)
                    Functions.ORD -> foldOrd(instruction)
                    else -> instruction
                }
            }

            // Skip if it is not a binary operation
            if (instruction !is TacAssignBinOp) return instruction

            val (v, op, lhs, rhs) = instruction

            if (lhs is IntImm && rhs is IntImm) {
                return when (op) {
                    BinaryOp.MULT -> TacAssignValue(v, IntImm(lhs.value * rhs.value))
                    BinaryOp.DIV -> TacAssignValue(v, IntImm(lhs.value / rhs.value))
                    BinaryOp.MOD -> TacAssignValue(v, IntImm(lhs.value % rhs.value))
                    BinaryOp.PLUS -> TacAssignValue(v, IntImm(lhs.value + rhs.value))
                    BinaryOp.MINUS -> TacAssignValue(v, IntImm(lhs.value - rhs.value))
                    BinaryOp.GT -> TacAssignValue(v, BoolImm(lhs.value > rhs.value))
                    BinaryOp.GTE -> TacAssignValue(v, BoolImm(lhs.value >= rhs.value))
                    BinaryOp.LT -> TacAssignValue(v, BoolImm(lhs.value < rhs.value))
                    BinaryOp.LTE -> TacAssignValue(v, BoolImm(lhs.value <= rhs.value))
                    BinaryOp.EQUALS -> TacAssignValue(v, BoolImm(lhs.value == rhs.value))
                    BinaryOp.NOT_EQUALS -> TacAssignValue(v, BoolImm(lhs.value != rhs.value))
                    else -> instruction
                }
            }

            if (lhs is CharImm && rhs is CharImm) {
                return when (op) {
                    BinaryOp.GT -> TacAssignValue(v, BoolImm(lhs.value > rhs.value))
                    BinaryOp.GTE -> TacAssignValue(v, BoolImm(lhs.value >= rhs.value))
                    BinaryOp.LT -> TacAssignValue(v, BoolImm(lhs.value < rhs.value))
                    BinaryOp.LTE -> TacAssignValue(v, BoolImm(lhs.value <= rhs.value))
                    BinaryOp.EQUALS -> TacAssignValue(v, BoolImm(lhs.value == rhs.value))
                    BinaryOp.NOT_EQUALS -> TacAssignValue(v, BoolImm(lhs.value != rhs.value))
                    else -> instruction
                }
            }

            if (lhs is BoolImm && rhs is BoolImm) {
                return when (op) {
                    BinaryOp.AND -> TacAssignValue(v, BoolImm(lhs.value && rhs.value))
                    BinaryOp.OR -> TacAssignValue(v, BoolImm(lhs.value || rhs.value))
                    else -> instruction
                }
            }

            return instruction
        }

        private fun foldChr(instruction: TacAssignCall): ThreeAddressCode {
            assert(instruction.args.size == 1)
            val arg = instruction.args[0]

            if (arg is IntImm) return TacAssignValue(instruction.dest, CharImm(arg.value.toChar()))

            return instruction
        }

        private fun foldOrd(instruction: TacAssignCall): ThreeAddressCode {
            assert(instruction.args.size == 1)
            val arg = instruction.args[0]

            if (arg is CharImm) return TacAssignValue(instruction.dest, IntImm(arg.value.code))

            return instruction
        }
    }
}

class OperatorStrengthReduction {
    companion object {
        fun apply(instruction: ThreeAddressCode): ThreeAddressCode {
            // Skip if it is not a binary operation
            if (instruction !is TacAssignBinOp) return instruction

            val (v, op, lhs, rhs) = instruction

            if (op == BinaryOp.MULT) {
                if (lhs is IntImm) {
                    // (v = 2 * a) => v = a + a
                    if (lhs.value == 2) return TacAssignBinOp(v, BinaryOp.PLUS, rhs, rhs)

                    // v = (constant multiple of 2) * a => v = a << n
                    if (isPowerOfTwo(lhs.value)) {
                        val pow = getExponentOfBaseTwo(lhs.value)
                        return TacAssignCall(v, Functions.LSL, rhs, IntImm(pow))
                    }
                }

                // (v = a * 2) => v = a + a
                if (rhs is IntImm) {
                    if (rhs.value == 2) return TacAssignBinOp(v, BinaryOp.PLUS, lhs, lhs)

                    // v = (constant multiple of 2) * a => v = a << n
                    if (isPowerOfTwo(rhs.value)) {
                        val pow = getExponentOfBaseTwo(rhs.value)
                        return TacAssignCall(v, Functions.LSL, lhs, IntImm(pow))
                    }
                }
            }

            if (op == BinaryOp.DIV) {
                if (rhs is IntImm) {
                    // v = a / (constant multiple of 2) => v = a >> n
                    if (isPowerOfTwo(rhs.value)) {
                        val pow = getExponentOfBaseTwo(rhs.value)
                        return TacAssignCall(v, Functions.ASR, lhs, IntImm(pow))
                    }
                }
            }

            return instruction
        }

        private fun isPowerOfTwo(x: Int): Boolean {
            return (x != 0) && ((x and (x - 1)) == 0)
        }

        private fun getExponentOfBaseTwo(x: Int): Int {
            return (ln(x.toDouble()) / ln(2.0)).roundToInt()
        }
    }
}

/**
 * Apply constant propagation within a basic block.
 */
class LocalConstantPropagation {
    companion object {
        fun apply(instructions: List<ThreeAddressCode>): List<ThreeAddressCode> {
            // Store a mapping of variables to immediate values
            val assignedMemo = mutableMapOf<TacVar, Imm>()
            val simplifiedInstructions = mutableListOf<ThreeAddressCode>()

            for (inst in instructions) {
                // Check if operands of an instruction are defined and substitute if so,
                // while updating the variable in assignedMemo
                val simplifiedInst = when (inst) {
                    is TacAssignBinOp -> propagateBinOp(inst, assignedMemo)
                    is TacAllocate -> propagateAlloc(inst, assignedMemo)
                    is TacAssignCall -> propagateAssignCall(inst, assignedMemo)
                    is TacAssignValue -> propagateAssignValue(inst, assignedMemo)
                    is TacBranchIf -> propagateBranchIf(inst, assignedMemo)
                    is TacCall -> propagateCall(inst, assignedMemo)
                    is TacLoad -> propagateLoad(inst, assignedMemo)
                    is TacStore -> propagateStore(inst, assignedMemo)
                    else -> inst
                }

                // Add (potentially constant propagated) instruction
                simplifiedInstructions.add(simplifiedInst)
            }

            return simplifiedInstructions
        }

        private fun propagateBinOp(
            instruction: TacAssignBinOp,
            assignedMemo: Map<TacVar, Imm>
        ): ThreeAddressCode {
            var (v, op, lhs, rhs) = instruction

            // Pattern match LHS
            if (lhs is TacVar && assignedMemo[lhs] is Imm) {
                lhs = assignedMemo[lhs] as Imm
            }

            // Pattern match RHS
            if (rhs is TacVar && assignedMemo[rhs] is Imm) {
                rhs = assignedMemo[rhs] as Imm
            }

            return TacAssignBinOp(v, op, lhs, rhs)
        }

        private fun propagateAlloc(instruction: TacAllocate, assignedMemo: Map<TacVar, Imm>): ThreeAddressCode {
            var (v, amount) = instruction

            if (amount is TacVar && assignedMemo[amount] is Imm) {
                amount = assignedMemo[amount] as Imm
            }

            return TacAllocate(v, amount)
        }

        private fun propagateAssignCall(
            instruction: TacAssignCall,
            assignedMemo: Map<TacVar, Imm>
        ): ThreeAddressCode {
            val simplifiedArgs = mutableListOf<TacOperand>()

            for (arg in instruction.args) {
                if (arg is TacVar && assignedMemo[arg] is Imm) {
                    simplifiedArgs.add(assignedMemo[arg] as Imm)
                } else {
                    simplifiedArgs.add(arg)
                }
            }

            return TacAssignCall(instruction.dest, instruction.f, *simplifiedArgs.toTypedArray())
        }

        private fun propagateAssignValue(instruction: TacAssignValue, assignedMemo: MutableMap<TacVar, Imm>): ThreeAddressCode {
            val (reg, value) = instruction
            if (value is Imm) assignedMemo[reg] = value
            return instruction
        }

        private fun propagateBranchIf(instruction: TacBranchIf, assignedMemo: Map<TacVar, Imm>): ThreeAddressCode {
            var (cond, block) = instruction
            if (cond is TacVar && assignedMemo[cond] is BoolImm) {
                cond = assignedMemo[cond] as BoolImm
            }
            return TacBranchIf(cond, block)
        }

        private fun propagateCall(instruction: TacCall, assignedMemo: Map<TacVar, Imm>): ThreeAddressCode {
            val simplifiedArgs = mutableListOf<TacOperand>()

            for (arg in instruction.args) {
                if (arg is TacVar && assignedMemo[arg] is Imm) {
                    simplifiedArgs.add(assignedMemo[arg] as Imm)
                } else {
                    simplifiedArgs.add(arg)
                }
            }

            return TacCall(instruction.f, *simplifiedArgs.toTypedArray())
        }

        private fun propagateLoad(instruction: TacLoad, assignedMemo: Map<TacVar, Imm>): ThreeAddressCode {
            var (v, addr) = instruction

            if (addr is TacVar && assignedMemo[addr] is Imm) {
                addr = assignedMemo[addr] as Imm
            }

            return TacLoad(v, addr)
        }

        private fun propagateStore(instruction: TacStore, assignedMemo: Map<TacVar, Imm>): ThreeAddressCode {
            var (v, addr) = instruction

            if (addr is TacVar && assignedMemo[addr] is Imm) {
                addr = assignedMemo[addr] as Imm
            }

            return TacStore(v, addr)
        }
    }
}

/**
 * Remove x = x.
 */
class RemoveRedundantAssignments {
    companion object {
        fun apply(instructions: List<ThreeAddressCode>): List<ThreeAddressCode> {
            val simplifiedInstructions = mutableListOf<ThreeAddressCode>()

            for (inst in instructions) {
                if (inst is TacAssignValue) {
                    val (reg, value) = inst
                    if (reg == value) continue
                }
                simplifiedInstructions.add(inst)
            }

            return simplifiedInstructions
        }
    }
}

/**
 * Join two statements where a variable is assigned the result of a temporary.
 * t = a + b; v = t => v = a + b
 *
 * The current implementation allows a list of redundant assignments to collapse
 * down into a single assignment; ie:
 * t1 = a + b; t2 = t1; t3 = t2; ... ; v = tn => v = a + b
 */
class RemoveTemporaries {
    companion object {
        fun apply(instructions: List<ThreeAddressCode>): List<ThreeAddressCode> {
            if (instructions.size < 2) return instructions

            val simplifiedInstructions = LinkedList(instructions)

            val it = simplifiedInstructions.listIterator()
            // Previous instruction
            var prev = it.next()

            // Traverse and combine redundant assignments
            while (it.hasNext()) {
                // Current instruction
                val curr = it.next()

                if (curr is TacAssignValue) {
                    val (v, t) = curr
                    when (prev) {
                        is TacAllocate -> {
                            val (reg, amount) = prev
                            if (reg == t) {
                                val combineInst = TacAllocate(v, amount)
                                removeLastTwoElements(it)
                                it.add(combineInst)
                                it.previous()
                            }
                        }
                        is TacAssignBinOp -> {
                            val (reg, op, lhs, rhs) = prev
                            if (reg == t) {
                                val combineInst = TacAssignBinOp(v, op, lhs, rhs)
                                removeLastTwoElements(it)
                                it.add(combineInst)
                                it.previous()
                            }
                        }
                        is TacAssignCall -> {
                            if (prev.dest == t) {
                                val combineInst = TacAssignCall(v, prev.f, *prev.args)
                                removeLastTwoElements(it)
                                it.add(combineInst)
                                it.previous()
                            }
                        }
                        is TacAssignValue -> {
                            val (reg, value) = prev
                            if (reg == t) {
                                val combineInst = TacAssignValue(v, value)
                                removeLastTwoElements(it)
                                it.add(combineInst)
                                it.previous()
                            }
                        }
                        is TacLoad -> {
                            val (reg, value) = prev
                            if (reg == t) {
                                val combineInst = TacLoad(v, value)
                                removeLastTwoElements(it)
                                it.add(combineInst)
                                it.previous()
                            }
                        }
                        is TacStore -> {
                            val (from, to) = prev
                            if (to == t) {
                                val combineInst = TacLoad(from, v)
                                removeLastTwoElements(it)
                                it.add(combineInst)
                                it.previous()
                            }
                        }
                    }
                }

                prev = curr
            }

            return simplifiedInstructions
        }

        private fun <T> removeLastTwoElements(it: MutableListIterator<T>) {
            it.previous()
            it.previous()
            it.remove()
            it.next()
            it.remove()
        }
    }
}

fun main() {
    val var1 = TacVar(1, BasicType.IntType)
    val var2 = TacVar(2, BasicType.IntType)
    val var3 = TacVar(3, BasicType.IntType)
    val var4 = TacVar(4, BasicType.IntType)
    val var5 = TacVar(5, BasicType.IntType)
    val var6 = TacVar(6, BasicType.IntType)
    val instructions = listOf(
        TacAssignBinOp(var1, BinaryOp.PLUS, IntImm(1), IntImm(1)),
        TacAssignValue(var2, var1),
        TacAssignValue(var3, var2),
        TacAssignValue(var4, var3),
        TacAssignBinOp(var5, BinaryOp.PLUS, IntImm(1), IntImm(1)),
        TacAssignValue(var6, var5)
    )
    val simplify = RemoveTemporaries.apply(instructions)
    println(simplify)

    val redundant = listOf(
        TacAssignValue(var1, var1),
        TacAssignValue(var2, var2)
    )
    println(RemoveRedundantAssignments.apply(redundant))
}
