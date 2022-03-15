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

    private val num = AtomicInteger(0)

    /**
     * Map register id to register (used to obtain result register)
     */
    private val regList: MutableMap<Int, Var> = HashMap()

    lateinit var resultRegister: Var
        private set

    /**
     * Mapping of variable to register storing its value
     */
    val varDefinedAt: MutableMap<Variable, Var> = mutableMapOf()

    fun newVar(type: Type): Var {
        val id = num.getAndIncrement()
        val v = Var(num.getAndIncrement(), type)
        regList[id] = v
        resultRegister = if (id == 0) {
            v
        } else {
            regList[id - 1]!!
        }
        return v
    }
}
