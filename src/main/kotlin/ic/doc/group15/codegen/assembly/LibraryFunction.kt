package ic.doc.group15.codegen.assembly

import ic.doc.group15.codegen.assembly.operand.BranchLabelOperand

class LibraryFunction private constructor(labelName: String) : BranchLabelOperand(labelName) {

    companion object {
        val SCANF = LibraryFunction("scanf")
        val PRINTF = LibraryFunction("printf")
        val FFLUSH = LibraryFunction("fflush")
        val EXIT = LibraryFunction("exit")
    }
}
