package ic.doc.group15.codegen.assembly

import ic.doc.group15.codegen.assembly.operand.BranchLabelOperand

class LibraryFunction private constructor(labelName: String) : BranchLabelOperand(labelName) {

    companion object {
        val SCANF = LibraryFunction("scanf")
        val PRINTF = LibraryFunction("printf")
        val FFLUSH = LibraryFunction("fflush")
        val EXIT = LibraryFunction("exit")
        val MALLOC = LibraryFunction("malloc")

        val AEABI_IDIV = LibraryFunction("__aeabi_idiv")
        val AEABI_IDIVMOD = LibraryFunction("__aeabi_idivmod")
    }
}
