package ic.doc.group15.ssa.cfg

import ic.doc.group15.ssa.BasicBlock
import ic.doc.group15.ssa.IRFunction

class CFGState {
    lateinit var irFunction: IRFunction
    lateinit var currentBlock: BasicBlock
}