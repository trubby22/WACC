package ic.doc.group15.ssa

import java.util.*

interface SsaTranslatable

class ControlFlowGraph : SsaTranslatable {

    private val functions: MutableList<IRFunction> = LinkedList()

    fun addFunction(func: IRFunction) = functions.add(func)

    fun getFunctions(): List<IRFunction> = functions
}
