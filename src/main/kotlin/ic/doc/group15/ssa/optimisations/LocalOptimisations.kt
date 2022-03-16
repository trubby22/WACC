package ic.doc.group15.ssa.optimisations

import ic.doc.group15.ast.BinaryOp
import ic.doc.group15.ssa.tac.*
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
            if (instruction is AssignCall) {
                return when (instruction.f) {
                    Functions.BANG -> checkNotIdentity(instruction)
                    else -> instruction
                }
            }

            // Skip if it is not a binary operation
            if (instruction !is AssignBinOp) return instruction

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

        private fun checkAdditionIdentity(instruction: AssignBinOp): ThreeAddressCode {
            val (v, _, lhs, rhs) = instruction

            // v = 0 + a => v = a
            if (lhs is IntImm && lhs.value == 0) return AssignValue(v, rhs)

            // v = a + 0 => v = a
            if (rhs is IntImm && rhs.value == 0) return AssignValue(v, lhs)

            return instruction
        }

        private fun checkSubtractionIdentity(instruction: AssignBinOp): ThreeAddressCode {
            val (v, _, lhs, rhs) = instruction

            // v = a - 0 => v = a
            if (rhs is IntImm && rhs.value == 0) return AssignValue(v, lhs)

            // v = 0 - a => v = -a
            if (lhs is IntImm && rhs is IntImm && lhs.value == 0) return AssignValue(v, IntImm(-rhs.value))

            // v = a - a => v = 0
            if (lhs is Var && rhs is Var && lhs.id == rhs.id) return AssignValue(v, IntImm(0))

            return instruction
        }

        private fun checkMultiplicationIdentity(instruction: AssignBinOp): ThreeAddressCode {
            val (v, _, lhs, rhs) = instruction

            // v = 1 * a => v = a
            if (lhs is IntImm) {
                // v = 1 * a => v = a
                if (lhs.value == 1) return AssignValue(v, rhs)

                // v = 0 * a => v = 0
                if (lhs.value == 0) return AssignValue(v, IntImm(0))

                // v = -1 * a => v = -a
                if (rhs is IntImm && lhs.value == -1) return AssignValue(v, IntImm(-rhs.value))
            }

            if (rhs is IntImm) {
                // v = a * 1 => v = a
                if (rhs.value == 1) return AssignValue(v, lhs)

                // v = a * 0 => v = 0
                if (rhs.value == 0) return AssignValue(v, IntImm(0))

                // v = a * -1 => v = -a
                if (lhs is IntImm && rhs.value == -1) return AssignValue(v, IntImm(-lhs.value))
            }

            return instruction
        }

        private fun checkDivisionIdentity(instruction: AssignBinOp): ThreeAddressCode {
            val (v, _, lhs, rhs) = instruction

            if (rhs is IntImm) {
                // v = a / 1 => v = a
                if (rhs.value == 1) return AssignValue(v, lhs)

                // v = a / -1 => v = -a
                if (lhs is IntImm && rhs.value == -1) return AssignValue(v, IntImm(-lhs.value))
            }

            return instruction
        }

        private fun checkEqualsIdentity(instruction: AssignBinOp): ThreeAddressCode {
            val (v, _, lhs, rhs) = instruction

            if (lhs is IntImm && rhs is IntImm) {
                return AssignValue(v, BoolImm(lhs.value == rhs.value))
            }

            if (lhs is CharImm && rhs is CharImm) {
                return AssignValue(v, BoolImm(lhs.value == rhs.value))
            }

            if (lhs is BoolImm && rhs is BoolImm) {
                return AssignValue(v, BoolImm(lhs.value == rhs.value))
            }

            if (lhs is StrImm && rhs is StrImm) {
                return AssignValue(v, BoolImm(lhs.value == rhs.value))
            }

            if (lhs is Var && rhs is Var) {
                return AssignValue(v, BoolImm(lhs.id == rhs.id))
            }

            return instruction
        }

        private fun checkNotEqualsIdentity(instruction: AssignBinOp): ThreeAddressCode {
            val (v, _, lhs, rhs) = instruction

            if (lhs is IntImm && rhs is IntImm) {
                return AssignValue(v, BoolImm(lhs.value != rhs.value))
            }

            if (lhs is CharImm && rhs is CharImm) {
                return AssignValue(v, BoolImm(lhs.value != rhs.value))
            }

            if (lhs is BoolImm && rhs is BoolImm) {
                return AssignValue(v, BoolImm(lhs.value != rhs.value))
            }

            if (lhs is StrImm && rhs is StrImm) {
                return AssignValue(v, BoolImm(lhs.value == rhs.value))
            }

            if (lhs is Var && rhs is Var) {
                return AssignValue(v, BoolImm(lhs.id != rhs.id))
            }

            return instruction
        }

        private fun checkAndIdentity(instruction: AssignBinOp): ThreeAddressCode {
            val (v, _, lhs, rhs) = instruction

            // v = (true && a) => v = a
            if (lhs is BoolImm && lhs.value) return AssignValue(v, rhs)

            // v = (false && a) => v = false
            if (lhs is BoolImm && !lhs.value) return AssignValue(v, BoolImm(false))

            // v = (a && true) => v = a
            if (rhs is BoolImm && rhs.value) return AssignValue(v, rhs)

            // v = (a && false) => v = false
            if (rhs is BoolImm && !rhs.value) return AssignValue(v, BoolImm(false))

            // v = (a && a) => v = a
            if (lhs is Var && rhs is Var && lhs.id == rhs.id) return AssignValue(v, lhs)

            return instruction
        }

        private fun checkOrIdentity(instruction: AssignBinOp): ThreeAddressCode {
            val (v, _, lhs, rhs) = instruction

            // v = (true || a) => v = true
            if (lhs is BoolImm && lhs.value) return AssignValue(v, BoolImm(true))

            // v = (false || a) => v = a
            if (lhs is BoolImm && !lhs.value) return AssignValue(v, rhs)

            // v = (a || true) => v = true
            if (rhs is BoolImm && rhs.value) return AssignValue(v, BoolImm(true))

            // v = (a || false) => v = a
            if (rhs is BoolImm && !rhs.value) return AssignValue(v, lhs)

            // v = (a || a) => v = a
            if (lhs is Var && rhs is Var && lhs.id == rhs.id) return AssignValue(v, lhs)

            return instruction
        }

        private fun checkNotIdentity(instruction: AssignCall): ThreeAddressCode {
            assert(instruction.args.size == 1)
            val arg = instruction.args[0]

            if (arg is BoolImm) {
                return AssignValue(instruction.reg, BoolImm(!arg.value))
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
            if (instruction is AssignCall) {
                return when (instruction.f) {
                    Functions.CHR -> foldChr(instruction)
                    Functions.ORD -> foldOrd(instruction)
                    else -> instruction
                }
            }

            // Skip if it is not a binary operation
            if (instruction !is AssignBinOp) return instruction

            val (v, op, lhs, rhs) = instruction

            if (lhs is IntImm && rhs is IntImm) {
                return when (op) {
                    BinaryOp.MULT -> AssignValue(v, IntImm(lhs.value * rhs.value))
                    BinaryOp.DIV -> AssignValue(v, IntImm(lhs.value / rhs.value))
                    BinaryOp.MOD -> AssignValue(v, IntImm(lhs.value % rhs.value))
                    BinaryOp.PLUS -> AssignValue(v, IntImm(lhs.value + rhs.value))
                    BinaryOp.MINUS -> AssignValue(v, IntImm(lhs.value - rhs.value))
                    BinaryOp.GT -> AssignValue(v, BoolImm(lhs.value > rhs.value))
                    BinaryOp.GTE -> AssignValue(v, BoolImm(lhs.value >= rhs.value))
                    BinaryOp.LT -> AssignValue(v, BoolImm(lhs.value < rhs.value))
                    BinaryOp.LTE -> AssignValue(v, BoolImm(lhs.value <= rhs.value))
                    BinaryOp.EQUALS -> AssignValue(v, BoolImm(lhs.value == rhs.value))
                    BinaryOp.NOT_EQUALS -> AssignValue(v, BoolImm(lhs.value != rhs.value))
                    else -> instruction
                }
            }

            if (lhs is CharImm && rhs is CharImm) {
                return when (op) {
                    BinaryOp.GT -> AssignValue(v, BoolImm(lhs.value > rhs.value))
                    BinaryOp.GTE -> AssignValue(v, BoolImm(lhs.value >= rhs.value))
                    BinaryOp.LT -> AssignValue(v, BoolImm(lhs.value < rhs.value))
                    BinaryOp.LTE -> AssignValue(v, BoolImm(lhs.value <= rhs.value))
                    BinaryOp.EQUALS -> AssignValue(v, BoolImm(lhs.value == rhs.value))
                    BinaryOp.NOT_EQUALS -> AssignValue(v, BoolImm(lhs.value != rhs.value))
                    else -> instruction
                }
            }

            if (lhs is BoolImm && rhs is BoolImm) {
                return when (op) {
                    BinaryOp.AND -> AssignValue(v, BoolImm(lhs.value && rhs.value))
                    BinaryOp.OR -> AssignValue(v, BoolImm(lhs.value || rhs.value))
                    else -> instruction
                }
            }

            return instruction
        }

        private fun foldChr(instruction: AssignCall): ThreeAddressCode {
            assert(instruction.args.size == 1)
            val arg = instruction.args[0]

            if (arg is IntImm) return AssignValue(instruction.reg, CharImm(arg.value.toChar()))

            return instruction
        }

        private fun foldOrd(instruction: AssignCall): ThreeAddressCode {
            assert(instruction.args.size == 1)
            val arg = instruction.args[0]

            if (arg is CharImm) return AssignValue(instruction.reg, IntImm(arg.value.code))

            return instruction
        }
    }
}

class OperatorStrengthReduction {
    companion object {
        fun apply(instruction: ThreeAddressCode): ThreeAddressCode {
            // Skip if it is not a binary operation
            if (instruction !is AssignBinOp) return instruction

            val (v, op, lhs, rhs) = instruction

            if (op == BinaryOp.MULT) {
                if (lhs is IntImm) {
                    // (v = 2 * a) => v = a + a
                    if (lhs.value == 2) return AssignBinOp(v, BinaryOp.PLUS, rhs, rhs)

                    // v = (constant multiple of 2) * a => v = a << n
                    if (isPowerOfTwo(lhs.value)) {
                        val pow = getExponentOfBaseTwo(lhs.value)
                        return AssignCall(v, Functions.LSL, rhs, IntImm(pow))
                    }
                }

                // (v = 2 * a) or (v = a * 2) => v = a + a
                if (rhs is IntImm) {
                    if (rhs.value == 2) {
                        return AssignBinOp(v, BinaryOp.PLUS, lhs, lhs)
                    }

                    // v = (constant multiple of 2) * a => v = a << n
                    if (isPowerOfTwo(rhs.value)) {
                        val pow = getExponentOfBaseTwo(rhs.value)
                        return AssignCall(v, Functions.LSL, lhs, IntImm(pow))
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