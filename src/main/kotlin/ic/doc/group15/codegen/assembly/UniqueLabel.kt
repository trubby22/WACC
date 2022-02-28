package ic.doc.group15.codegen.assembly

import java.util.concurrent.atomic.AtomicInteger

open class UniqueLabel(private val startingString: String) {

    private val counter = AtomicInteger(0)

    fun generate(): String = startingString + "${counter.getAndIncrement()}"
}

class UniqueStringLabel : UniqueLabel("msg_")

class UniqueBranchLabel : UniqueLabel("L")
