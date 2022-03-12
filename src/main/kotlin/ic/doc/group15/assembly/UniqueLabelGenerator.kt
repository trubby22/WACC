package ic.doc.group15.assembly

import java.util.concurrent.atomic.AtomicInteger

open class UniqueLabelGenerator(private val startingString: String) {

    private val counter = AtomicInteger(0)

    fun generate(): String {
        return StringBuilder(startingString).append("${counter.getAndIncrement()}").toString()
    }
}

class UniqueStringLabelGenerator : UniqueLabelGenerator("msg_")

class UniqueBranchLabelGenerator : UniqueLabelGenerator("L")
