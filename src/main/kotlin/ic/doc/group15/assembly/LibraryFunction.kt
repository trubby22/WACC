package ic.doc.group15.assembly

import ic.doc.group15.assembly.operand.BranchLabelOperand

class LibraryFunction private constructor(labelName: String) : BranchLabelOperand(labelName) {

    companion object {
        val SCANF = LibraryFunction("scanf")
        val PRINTF = LibraryFunction("printf")
        val PUTS = LibraryFunction("puts")
        val PUTCHAR = LibraryFunction("putchar")
        val FFLUSH = LibraryFunction("fflush")
        val EXIT = LibraryFunction("exit")
        val MALLOC = LibraryFunction("malloc")
        val FREE = LibraryFunction("free")
        val AEABI_IDIV = LibraryFunction("__aeabi_idiv")
        val AEABI_IDIVMOD = LibraryFunction("__aeabi_idivmod")
    }
}
