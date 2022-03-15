package ic.doc.group15.ssa.cfg

import ic.doc.group15.ssa.BasicBlock
import ic.doc.group15.ssa.IRFunction
import ic.doc.group15.ssa.tac.Var
import ic.doc.group15.type.Type
import ic.doc.group15.type.Variable
import java.util.concurrent.atomic.AtomicInteger

class CfgState {

    lateinit var irFunction: IRFunction
    lateinit var currentBlock: BasicBlock

    /**
     * Map register id to register (used to obtain result register)
     */
    private val varList: MutableList<Var> = ArrayList()

    lateinit var resultRegister: Var
        private set

    /**
     * Mapping of variable to register storing its value
     * TODO("Populate the map properly")
     */
    val varDefinedAt: MutableMap<Variable, Var> = mutableMapOf()

    fun newVar(type: Type): Var {
        val v = Var(varList.size, type)
        resultRegister = if (varList.isEmpty()) {
            v
        } else {
            varList[varList.lastIndex - 1]
        }
        varList.add(v)
        return v
    }
}
