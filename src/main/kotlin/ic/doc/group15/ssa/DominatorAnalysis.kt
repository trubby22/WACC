package ic.doc.group15.ssa

import java.util.concurrent.atomic.AtomicInteger

class DomIdState {
    private val counter = AtomicInteger()

    fun generateId(): Int {
        return counter.getAndIncrement()
    }
}

data class DominanceAnalysisResult(val immediateDominator: Map<Block, Block>, val dominanceFrontier: Map<Block, Set<Block>>)

/**
 * Dominator tree and dominance analysis. Algorithm to compute immediate dominators
 * is based on the improved iterative algorithm described in the paper
 * "A Simple, Fast Dominance Algorithm" by Keith D. Cooper, Timothy
 * J. Harvey and Ken Kennedy; algorithm to find dominance frontier is from the
 * same paper adapted from algorithm proposed by Cytron et al.
 *
 * Note that other than the root node, the set of dominators for a node/block
 * can be calculated by:
 * Dom(b) = {b} 'union' idom(b) 'union' idom(idom(b)) ... {root}
 */

class DominanceAnalysis {
    companion object {
        const val UNINITIALISED_STATE = -1
    }

    /**
     * Return immediate dominator and compute the set of dominance frontier for each node.
     */
    fun performDominanceAnalysis(root: Block): DominanceAnalysisResult {
        // Map node to unique id number
        val postorderIdMap: Map<Block, Int> = generatePostorderNumbering(root)
        val nodeCount = postorderIdMap.size

        // Get list of blocks/nodes in reverse postorder id
        val idToBlockMap = postorderIdMap.entries.associateBy ({ it.value }, { it.key })
        val revOrderBlocksExceptRoot = sortNodeInReverseId(idToBlockMap).toMutableList()
        // Remove last element (root visited last in postorder traversal)
        assert(revOrderBlocksExceptRoot.removeLast() == root)

        // Use primitive array to represent id of block
        // Each entry doms[b] for the id of a block/node b contains the id of idom(b)
        val doms = IntArray(nodeCount) { UNINITIALISED_STATE }

        // Apply algorithm specified in paper
        doms[postorderIdMap[root]!!] = postorderIdMap[root]!!
        var modified = true

        while (modified) {
            modified = false
            for (node in revOrderBlocksExceptRoot) {
                val block = postorderIdMap[node]!!
                // All nodes except root guaranteed to hold a set of predecessors
                val predecessors = node.getPredecessors()!!.map { postorderIdMap[it]!! }.toMutableList()

                // Pick a predecessor of current node and treat it as processed
                // All nodes guaranteed to have at least one predecessor
                var new_idom: Int = predecessors.removeFirst()

                // Process all other predecessors except initial new_idom
                for (pred in predecessors) {
                    // If doms[pred] has been computed
                    if (doms[pred] != UNINITIALISED_STATE) {
                        new_idom = intersect(pred, new_idom, doms)
                    }
                }

                if (doms[block] != new_idom) {
                    doms[block] = new_idom
                    modified = true
                }
            }
        }

        // Map doms array back to block
        val immDom = doms.associateBy({ idToBlockMap[it]!! }, { idToBlockMap[doms[it]]!! })

        // Initialise map of sets to compute dominance frontier sets for each block
        val domFrontier = postorderIdMap.entries.associateBy ({ it.key }, { mutableSetOf<Block>() })

        // Compute dominance frontier algorithm
        val allNodes = postorderIdMap.keys
        for (node in allNodes) {
            val block = postorderIdMap[node]!!
            val predecessors = node.getPredecessors() ?: continue

            if (predecessors.size >= 2) {
                for (pred in predecessors) {
                    var runner = postorderIdMap[pred]!!
                    while (runner != doms[block]) {
                        domFrontier[idToBlockMap[runner]!!]!!.add(node)
                        runner = doms[runner]
                    }
                }
            }
        }


        return DominanceAnalysisResult(immDom, domFrontier)
    }

    /**
     * Helper function, as defined in the paper "A Simple, Fast Dominance Algorithm".
     */
    private fun intersect(b1: Int, b2: Int, doms: IntArray): Int {
        var (finger1, finger2) = Pair(b1, b2)

        while (finger1 != finger2) {
            while (finger1 < finger2) {
                finger1 = doms[finger1]
            }
            while (finger2 < finger1) {
                finger2 = doms[finger2]
            }
        }

        return finger1
    }

    private fun generatePostorderNumbering(root: Block): Map<Block, Int> {
        val map = mutableMapOf<Block, Int>()
        generatePostorderNumbering(root, map, DomIdState())
        return map
    }

    // We label each block/node by a unique id within the range [0, number of blocks)
    private fun generatePostorderNumbering(root: Block, map: MutableMap<Block, Int>, domIdState: DomIdState) {
        val successors = root.getSuccessors()
        if (successors?.isNotEmpty() == true) {
            for (succ in successors) {
                generatePostorderNumbering(succ, map, domIdState)
            }
        }
        map[root] = domIdState.generateId()
    }

    // Pre: map is a one-to-one mapping
    private fun sortNodeInReverseId(map: Map<Int, Block>): List<Block> {
        val nodeCount = map.size
        return (nodeCount downTo 0).map { map[it]!! }.toList()
    }
}