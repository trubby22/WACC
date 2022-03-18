package ic.doc.group15.ssa.tac

import ic.doc.group15.ssa.*

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
      val pred = mutableMapOf<ThreeAddressCode, Set<ThreeAddressCode>>()
      if (function.basicBlocks.isEmpty()) return pred

      val root = function.basicBlocks[0]

      val rootInstList = root.getInstructionList()
      if (rootInstList.isEmpty()) return pred

      // First instruction has no pred
      findPred(root, pred, emptySet())

      return pred
    }

    private fun findPred(
      block: BasicBlock,
      pred: MutableMap<ThreeAddressCode, Set<ThreeAddressCode>>,
      predOfFirstInst: Set<ThreeAddressCode>
    ) {
      // Pass pred to first instruction in next block if current block is
      // empty
      val instList = block.getInstructionList()
      if (instList.isEmpty()) {
        for (succ in block.getSuccessors()) {
          if (succ is ExitBasicBlock) continue
          findPred(succ as BasicBlock, pred, predOfFirstInst)
        }
      }

      // List contains at least one instruction
      // Set pred for each instruction in current block
      if (pred[instList[0]] == null) {
        pred[instList[0]] = emptySet()
      }
      pred[instList[0]] = pred[instList[0]]!! union predOfFirstInst

      if (instList.size > 1) {
        for (i in 1 until instList.size) {
          pred[instList[i]] = setOf(instList[i - 1])
        }
      }

      for (succ in block.getSuccessors()) {
        if (succ is ExitBasicBlock) continue
        findPred(succ as BasicBlock, pred, pred[instList.last()]!!)
      }
    }

    private fun findSucc(function: IRFunction): Map<ThreeAddressCode, Set<ThreeAddressCode>> {
      val succ = mutableMapOf<ThreeAddressCode, Set<ThreeAddressCode>>()
      if (function.basicBlocks.isEmpty()) return succ

      val root = function.basicBlocks[0]

      val rootInstList = root.getInstructionList()
      if (rootInstList.isEmpty()) return succ

      findSucc(root, succ, null)

      return succ
    }

    private fun findSucc(
      block: BasicBlock,
      succMap: MutableMap<ThreeAddressCode, Set<ThreeAddressCode>>,
      lastInstToFindSucc: ThreeAddressCode?
    ) {
      val instList = block.getInstructionList()
      if (instList.isEmpty()) {
        for (succ in block.getSuccessors()) {
          if (succ is ExitBasicBlock) continue
          findSucc(succ as BasicBlock, succMap, lastInstToFindSucc)
        }
      }

      // List contains at least one instruction
      // Set succ for each instruction in current block
      if (lastInstToFindSucc != null) {
        if (succMap[lastInstToFindSucc] == null) {
          succMap[lastInstToFindSucc] = emptySet()
        }

        succMap[lastInstToFindSucc] =
          succMap[lastInstToFindSucc]!! union setOf(instList[0])
      }

      if (instList.size > 1) {
        for (i in 0 until instList.size - 1) {
          succMap[instList[i]] = setOf(instList[i + 1])
        }
      }

      for (succ in block.getSuccessors()) {
        if (succ is ExitBasicBlock) continue
        findSucc(succ as BasicBlock, succMap, instList.last())
      }
    }

    fun apply(function: IRFunction): LivenessResult {
      TODO()
    }
  }
}