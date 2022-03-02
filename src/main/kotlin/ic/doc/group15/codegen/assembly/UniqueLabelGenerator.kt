package ic.doc.group15.codegen.assembly

import java.util.concurrent.atomic.AtomicInteger

open class UniqueLabelGenerator(private val startingString: String) {

    private val counter = AtomicInteger(0)

    fun generate(): String = startingString + "${counter.getAndIncrement()}"
}

class UniqueStringLabelGenerator : UniqueLabelGenerator("msg_")

class UniqueBranchLabelGenerator : UniqueLabelGenerator("L")
