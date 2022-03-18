package ic.doc.group15.ssa.cfg

import ic.doc.group15.ssa.BasicBlock
import ic.doc.group15.ssa.IRFunction
import ic.doc.group15.ssa.tac.TacVar
import ic.doc.group15.type.Type
import ic.doc.group15.type.Variable

class CfgState {

    lateinit var irFunction: IRFunction
    lateinit var currentBlock: BasicBlock

    /**
     * Map register id to register (used to obtain result register)
     */
    private val varList: MutableList<TacVar> = ArrayList()

    lateinit var resultRegister: TacVar
        private set

    /**
     * Mapping of variable to register storing its value
     * TODO("Populate the map properly")
     */
    val varDefinedAt: MutableMap<Variable, TacVar> = mutableMapOf()

    fun newVar(type: Type): TacVar {
        val v = TacVar(varList.size, type)
        resultRegister = if (varList.isEmpty()) {
            v
        } else {
            varList[varList.lastIndex - 1]
        }
        varList.add(v)
        return v
    }

    fun varSet(): Set<TacVar> = varList.toSet()
}
