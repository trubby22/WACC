package ic.doc.group15.ssa.tac

import ic.doc.group15.assembly.BranchLabel
import ic.doc.group15.assembly.Instruction
import ic.doc.group15.assembly.operand.ArmRegister
import ic.doc.group15.assembly.operand.Register
import ic.doc.group15.ssa.BasicBlock
import ic.doc.group15.ssa.ExitBasicBlock
import ic.doc.group15.ssa.PseudoRegister
import ic.doc.group15.util.WORD
import java.util.*
import kotlin.collections.ArrayDeque
import kotlin.collections.set

typealias Interval = Pair<Int, Int>
data class LiveInterval(val v: PseudoRegister, val interval: Interval)

data class LivenessResult(
  val inOfVar: Map<Instruction, Set<PseudoRegister>>,
  val outOfVar: Map<Instruction, Set<PseudoRegister>>,
  val predOfInst: Map<Instruction, Set<Instruction>>,
  val succOfInst: Map<Instruction, Set<Instruction>>
)

data class RegAllocState(
  val availableReg: ArrayDeque<ArmRegister>,
  val regAssignment: MutableMap<LiveInterval, ArmRegister>,
  val stackAssignment: MutableMap<LiveInterval, Int>,
  val intervals: PriorityQueue<LiveInterval>,
  val active: PriorityQueue<LiveInterval>,
)

data class RegAllocResult(
  val regAssignment: Map<PseudoRegister, ArmRegister>,
  val stackAssignment: Map<PseudoRegister, Int>
)

class LinearScanRegAlloc {
  companion object {
    private fun findPred(blockToLabelMap: Map<BasicBlock, BranchLabel>): Map<Instruction, Set<Instruction>> {
      val pred = mutableMapOf<Instruction, Set<Instruction>>()
      if (blockToLabelMap.isEmpty()) return pred

      val root = blockToLabelMap.keys.first()

      val rootInstList = blockToLabelMap[root]!!.getLines()
      if (rootInstList.isEmpty()) return pred

      // First instruction has no pred
      findPred(root, blockToLabelMap, pred, emptySet())

      return pred
    }

    private fun findPred(
      block: BasicBlock,
      blockToLabelMap: Map<BasicBlock, BranchLabel>,
      pred: MutableMap<Instruction, Set<Instruction>>,
      predOfFirstInst: Set<Instruction>
    ) {
      // Pass pred to first instruction in next block if current block is
      // empty
      val instList = blockToLabelMap[block]!!.getLines()
      if (instList.isEmpty()) {
        for (succ in block.getSuccessors()) {
          if (succ is ExitBasicBlock) continue
          findPred(succ as BasicBlock, blockToLabelMap, pred, predOfFirstInst)
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
        findPred(succ as BasicBlock, blockToLabelMap, pred, pred[instList.last()]!!)
      }
    }

    private fun findSucc(blockToLabelMap: Map<BasicBlock, BranchLabel>): Map<Instruction, Set<Instruction>> {
      val succ = mutableMapOf<Instruction, Set<Instruction>>()
      if (blockToLabelMap.isEmpty()) return succ

      val root = blockToLabelMap.keys.first()

      val rootInstList = blockToLabelMap[root]!!.getLines()
      if (rootInstList.isEmpty()) return succ

      findSucc(root, blockToLabelMap, succ, null)

      return succ
    }

    private fun findSucc(
      block: BasicBlock,
      blockToLabelMap: Map<BasicBlock, BranchLabel>,
      succMap: MutableMap<Instruction, Set<Instruction>>,
      lastInstToFindSucc: Instruction?
    ) {
      val instList = blockToLabelMap[block]!!.getLines()

      if (instList.isEmpty()) {
        for (succ in block.getSuccessors()) {
          if (succ is ExitBasicBlock) continue
          findSucc(succ as BasicBlock, blockToLabelMap, succMap, lastInstToFindSucc)
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
        findSucc(succ as BasicBlock, blockToLabelMap, succMap, instList.last())
      }
    }

    /**
     * Efficient worklist algorithm as defined in
     * https://groups.seas.harvard.edu/courses/cs153/2019fa/lectures/Lec20-Dataflow-analysis.pdf
     */
    fun livenessAnalysis(blockToLabelMap: Map<BasicBlock, BranchLabel>): LivenessResult {
      val predOf = findPred(blockToLabelMap)
      val succOf = findSucc(blockToLabelMap)

      // Confirm the number of instructions is same
      assert(predOf.size == succOf.size)

      val allInsts = predOf.keys

      // Initialise set of variables live on entry for each instruction
      val inOf = allInsts.associateBy({ it },
        { emptySet<PseudoRegister>() }).toMutableMap()
      // Initialise set of variables live on exit for each instruction
      val outOf = HashMap(inOf)

      // Use a FIFO queue of instructions to be updated
      val worklist = ArrayDeque(allInsts)

      while (worklist.isNotEmpty()) {
        val inst = worklist.removeFirst()

        // Remember old inOf[inst]
        val oldIn = inOf[inst]!!

        // out[inst] := for all inst' in succ[inst], the result of union of in[inst']
        val newOut =
          succOf[inst]!!.fold(emptySet<PseudoRegister>()) { acc, elem -> acc union inOf[elem]!! }
        outOf[inst] = newOut

        // in[inst] := use[inst] union (out[inst] - def[inst])
        val newIn = inst.usesSet() union (newOut subtract inst.definesSet())
        inOf[inst] = newIn.map { r -> r as PseudoRegister }.toSet()

        // If worklist has changed, add new work to worklist
        if (oldIn != inOf[inst]) {
          for (pred in predOf[inst]!!) {
            worklist.addLast(pred)
          }
        }
      }

      return LivenessResult(inOf, outOf, predOf, succOf)
    }

    private fun sortInDFSOrder(root: BasicBlock): List<BasicBlock> {
      val visited = LinkedHashSet<BasicBlock>()

      dfs(root, visited)

      return visited.toList()
    }

    private fun dfs(block: BasicBlock, visited: MutableSet<BasicBlock>) {
      visited.add(block)

      for (succ in block.getSuccessors()) {
        if (succ is ExitBasicBlock) return
        if (!visited.contains(succ)) dfs(block, visited)
      }
    }

    /**
     * Compute the smallest subrange of the IR code containing all of
     * a variable's live ranges.
     */
    fun computeLivenessIntervals(
      blockToLabelMap: Map<BasicBlock, BranchLabel>
    ): Map<PseudoRegister, Interval> {
      val (inOf, _, _, _) = livenessAnalysis(blockToLabelMap)

      val root = blockToLabelMap.keys.first()
      val dfsOrder = sortInDFSOrder(root)

      // Initialise interval of all variables in DFS order; add line number to inst
      val instLineNum = mutableMapOf<Int, Instruction>()
      val intervals: MutableMap<PseudoRegister, Interval> = LinkedHashMap()
      var currLineNum = 1
      for (b in dfsOrder) {
        for (inst in blockToLabelMap[b]!!.getLines()) {
          // Add line number of instruction in DFS order
          instLineNum[currLineNum++] = inst
          val definedRegs = inst.definesSet()
          if (definedRegs.isEmpty()) continue

          val defReg = definedRegs.first() as PseudoRegister

          val pseudoReg = PseudoRegister(defReg.id)
          intervals[pseudoReg] = Pair(Int.MAX_VALUE, Int.MIN_VALUE)
        }
      }

      // Update interval of variables appearing in each instruction
      // TODO: Check correctness
      for ((line, inst) in instLineNum) {
        // Update define and uses variable
        for (v in inOf[inst]!!) {
          val (min, max) = intervals[v]!!
          intervals[v] = Pair(min.coerceAtMost(line), max.coerceAtLeast(line + 1))
        }
      }

      return intervals
    }

    private fun linearScanRegAlloc(
      blockToLabelMap: Map<BasicBlock, BranchLabel>,
      availableRegisters: ArrayDeque<ArmRegister>
    ): RegAllocState {
      // Sort live intervals in order of increasing start point
      val state = RegAllocState(
        availableRegisters,
        mutableMapOf(),
        mutableMapOf(),
        PriorityQueue<LiveInterval> { i1, i2 -> i1.interval.first - i2.interval.first },
        PriorityQueue<LiveInterval> { i1, i2 -> i1.interval.second - i2.interval.second }
      )

      state.intervals.addAll(
        computeLivenessIntervals(blockToLabelMap).map { LiveInterval(it.key, it.value) }
      )

      for (interval in state.intervals) {
        expireOldIntervals(interval, state)
        if (state.active.size == availableRegisters.size) {
          spillAtInterval(interval, state)
        } else {
          state.regAssignment[interval] = availableRegisters.removeFirst()
          state.active.add(interval)
        }
      }

      return state
    }

    private fun expireOldIntervals(interval: LiveInterval, state: RegAllocState) {
      // sort active in order of increasing end point
      for (activeInterval in state.active) {
        // endpoint[activeInterval] >= startpoint[interval]
        if (activeInterval.interval.second >= interval.interval.first) return
        state.active.remove(activeInterval)
        val freeReg = state.regAssignment.remove(activeInterval)
        state.availableReg.addLast(freeReg!!)
      }
    }

    private fun spillAtInterval(
      interval: LiveInterval,
      state: RegAllocState
    ) {
      val spill = state.active.last()
      // endpoint[spill] > endpoint[interval]
      if (spill.interval.second > interval.interval.second) {
        state.regAssignment[interval] = state.regAssignment[spill]!!
        // Assign new stack location (assuming offset 0 from arguments)
        state.stackAssignment[spill] = state.stackAssignment.size * WORD
        state.active.remove(spill)
        state.active.add(interval)
      } else {
        state.stackAssignment[interval] = state.stackAssignment.size * WORD
      }
    }

    fun apply(
      blockToLabelMap: Map<BasicBlock, BranchLabel>,
      availableRegisters: ArrayDeque<ArmRegister>): RegAllocResult {
      val result = linearScanRegAlloc(blockToLabelMap, availableRegisters)
      val register = result.regAssignment.entries.associateBy({it.key.v}, {it.value})
      val stack = result.stackAssignment.entries.associateBy({it.key.v}, {it.value})

      return RegAllocResult(register, stack)
    }
  }
}