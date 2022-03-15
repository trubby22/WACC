package ic.doc.group15.ssa.cfg

import ic.doc.group15.ssa.BasicBlock
import ic.doc.group15.ssa.IRFunction
import ic.doc.group15.ssa.tac.Var
import ic.doc.group15.type.Variable

class CFGState {
    lateinit var irFunction: IRFunction
    lateinit var currentBlock: BasicBlock
    // Map register id to register (used to obtain result register)
    val regList: MutableMap<Int, Var> = mutableMapOf()
    // Mapping of variable to register storing its value
    val varDefinedAt: MutableMap<Variable, Var> = mutableMapOf()
    // Mapping of counter for variables with same name but different declaration
    val varCounter: MutableMap<Variable, Int> = mutableMapOf()
    var smallestUnusedId = 0
}