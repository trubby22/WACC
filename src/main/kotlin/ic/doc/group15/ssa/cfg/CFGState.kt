package ic.doc.group15.ssa.cfg

import ic.doc.group15.ssa.BasicBlock
import ic.doc.group15.ssa.IRFunction
import ic.doc.group15.ssa.tac.Register
import ic.doc.group15.type.Variable

class CFGState {
    lateinit var irFunction: IRFunction
    lateinit var currentBlock: BasicBlock
    // Map register id to register (used to obtain result register)
    val regList: MutableMap<Int, Register> = mutableMapOf()
    // Mapping of variable to register storing its value
    val varInReg: MutableMap<Variable, Register> = mutableMapOf()
    var smallestUnusedId = 0
}