package ic.doc.group15.visitor

import ic.doc.group15.ssa.BidirectionalBlock
import ic.doc.group15.ssa.Block

/**
 * Dominator tree and dominance analysis. Algorithm to find dominator tree
 * is based on the improved iterative algorithm described in the paper
 * "A Simple, Fast Dominance Algorithm" by Keith D. Cooper, Timothy
 * J. Harvey and Ken Kennedy; algorithm to find dominance frontier is by
 * Cytron et al.
 */
class DomGenerator {

    fun generatePostorderNumbering(root: Block): Map<Block, Int> {
        val map = mutableMapOf<Block, Int>()
        return generatePostorderNumbering(root, map)
    }

    fun generatePostorderNumbering(root: Block, map: MutableMap<Block, Int>): Map<Block, Int> {
        TODO()
    }
}