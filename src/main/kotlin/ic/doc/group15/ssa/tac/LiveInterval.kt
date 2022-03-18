package ic.doc.group15.ssa.tac

import ic.doc.group15.ssa.BasicBlock
import ic.doc.group15.ssa.ExitBasicBlock
import ic.doc.group15.ssa.IRFunction

data class LivenessResult(
  val inOfVar: Map<ThreeAddressCode, Set<Var>>,
  val outOfVar: Map<ThreeAddressCode, Set<Var>>)

class LivenessAnalysis {
  companion object {
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

    /**
     * Efficient worklist algorithm as defined in
     * https://groups.seas.harvard.edu/courses/cs153/2019fa/lectures/Lec20-Dataflow-analysis.pdf
     */
    fun apply(function: IRFunction): LivenessResult {
      val predOf = findPred(function)
      val succOf = findSucc(function)

      // Confirm the number of instructions is same
      assert(predOf.size == succOf.size)

      val allInsts = predOf.keys

      // Initialise set of variables live on entry for each instruction
      val inOf = allInsts.associateBy({ it },
        { emptySet<Var>() }).toMutableMap()
      // Initialise set of variables live on exit for each instruction
      val outOf = HashMap(inOf)

      // Use a FIFO queue of instructions to be updated
      val worklist = ArrayDeque(allInsts)

      while (worklist.isNotEmpty()) {
        val inst = worklist.removeFirst()

        // Remember old inOf[inst]
        val oldIn = inOf[inst]!!

        // out[inst] := for all inst' in succ[inst], the result of union of in[inst']
        val newOut = succOf[inst]!!.fold(emptySet<Var>()) { acc, elem -> acc union inOf[elem]!! }
        outOf[inst] = newOut

        // in[inst] := use[inst] union (out[inst] - def[inst])
        val newIn = inst.usesSet() union (newOut subtract inst.definesSet())
        inOf[inst] = newIn

        // If worklist has changed, add new work to worklist
        if (oldIn != inOf[inst]) {
          for (pred in predOf[inst]!!) {
            worklist.addLast(pred)
          }
        }
      }

      return LivenessResult(inOf, outOf)
    }
  }
}