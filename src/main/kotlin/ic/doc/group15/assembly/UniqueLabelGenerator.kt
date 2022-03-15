package ic.doc.group15.assembly

import ic.doc.group15.util.UniqueGenerator

open class UniqueLabelGenerator(private val startingString: String) : UniqueGenerator<String>() {

    override fun generate(): String {
        return "$startingString${uniqueNum()}"
    }
}

class UniqueStringLabelGenerator : UniqueLabelGenerator("msg_")

class UniqueBranchLabelGenerator : UniqueLabelGenerator("L")
