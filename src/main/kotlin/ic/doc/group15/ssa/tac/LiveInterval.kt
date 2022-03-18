package ic.doc.group15.ssa.tac

import ic.doc.group15.assembly.operand.Register
import ic.doc.group15.ssa.BasicBlock
import ic.doc.group15.ssa.ExitBasicBlock
import ic.doc.group15.ssa.IRFunction
import ic.doc.group15.util.WORD
import java.util.*
import kotlin.collections.ArrayDeque
import kotlin.collections.set

typealias Interval = Pair<Int, Int>
data class LiveInterval(val v: TacVar, val interval: Interval)

data class LivenessResult(
  val inOfVar: Map<ThreeAddressCode, Set<TacVar>>,
  val outOfVar: Map<ThreeAddressCode, Set<TacVar>>,
  val predOfInst: Map<ThreeAddressCode, Set<ThreeAddressCode>>,
  val succOfInst: Map<ThreeAddressCode, Set<ThreeAddressCode>>
)

data class RegAllocState(
  val availableReg: ArrayDeque<Register>,
  val regAssignment: MutableMap<LiveInterval, Register>,
  val stackAssignment: MutableMap<LiveInterval, Int>,
  val intervals: PriorityQueue<LiveInterval>,
  val active: PriorityQueue<LiveInterval>,
)

data class RegAllocResult(
  val regAssignment: Map<TacVar, Register>,
  val stackAssignment: Map<TacVar, Int>
)

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
        { emptySet<TacVar>() }).toMutableMap()
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
          succOf[inst]!!.fold(emptySet<TacVar>()) { acc, elem -> acc union inOf[elem]!! }
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

      return LivenessResult(inOf, outOf, predOf, succOf)
    }

    private fun sortInDFSOrder(function: IRFunction): List<BasicBlock> {
      if (function.basicBlocks.isEmpty()) return emptyList()

      val root = function.basicBlocks[0]
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
    fun computeLivenessIntervals(function: IRFunction): Map<TacVar, Interval> {
      val (inOf, _, _, _) = apply(function)

      // Initialise interval of all variables
      val intervals: MutableMap<TacVar, Interval> =
        function.variableSet.associateBy({ it },
          { Pair(Int.MAX_VALUE, Int.MIN_VALUE) }).toMutableMap()

      // Update interval of variables appearing in each instruction
      // TODO: Check correctness
      val varsInOf = inOf.values.toList()
      for (i in varsInOf.indices) {
        // Update define and uses variable
        for (v in varsInOf[i]) {
          val (min, max) = intervals[v]!!
          intervals[v] = Pair(min.coerceAtMost(i), max.coerceAtLeast(i + 1))
        }
      }

      return intervals
    }

    private fun linearScanRegAlloc(
      function: IRFunction,
      availableRegisters: ArrayDeque<Register>
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
        computeLivenessIntervals(function).map { LiveInterval(it.key, it.value) }
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

    fun registerAllocation(function: IRFunction, availableReg: ArrayDeque<Register>): RegAllocResult {
      val result = linearScanRegAlloc(function, availableReg)
      val register = result.regAssignment.entries.associateBy({it.key.v}, {it.value})
      val stack = result.stackAssignment.entries.associateBy({it.key.v}, {it.value})

      return RegAllocResult(register, stack)
    }
  }
}