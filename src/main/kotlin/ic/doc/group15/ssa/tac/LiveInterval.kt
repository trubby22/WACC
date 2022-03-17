package ic.doc.group15.ssa.tac

import ic.doc.group15.ssa.IRFunction
import ic.doc.group15.ssa.Successor
import ic.doc.group15.ssa.SuccessorBlock

data class LivenessResult(val dummy: Int)

class LivenessAnalysis {
    companion object {
        // Variable defined by instruction
        private fun varAssignedTo(instruction: ThreeAddressCode): Var? {
            return when (instruction) {
                is Allocate -> instruction.reg
                is AssignBinOp -> instruction.reg
                is AssignCall -> instruction.reg
                is AssignValue -> instruction.reg
                is Load -> instruction.reg
                else -> null
            }
        }

        private fun findPred(function: IRFunction): Map<ThreeAddressCode, Set<ThreeAddressCode>> {
            val pred = mutableMapOf<ThreeAddressCode, MutableSet<ThreeAddressCode>>()
            if (function.basicBlocks.isEmpty()) return pred

            var root = function.basicBlocks[0]

            val rootInstList = root.getInstructionList()
            if (rootInstList.isEmpty()) return pred

            // First instruction has no pred
            findPred(root, pred, mutableSetOf())

            return pred
        }

        private fun findPred(
            block: Successor,
            pred: MutableMap<ThreeAddressCode, MutableSet<ThreeAddressCode>>,
            predOfFirstInst: MutableSet<ThreeAddressCode>
        ) {
        }


        fun apply(function: IRFunction): LivenessResult {
        TODO()
        }
    }
}