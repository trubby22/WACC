package ic.doc.group15.ssa.optimisations

import ic.doc.group15.ast.BinaryOp
import ic.doc.group15.ssa.tac.*
import ic.doc.group15.visitor.TranslatorVisitor

/**
 * Applies optimisation techniques local to each basic block in three address code form.
 */
class BinaryOperationIdentity {
    companion object {
        fun simplify(instruction: ThreeAddressCode): ThreeAddressCode {
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
    }
}