package ic.doc.group15.error.syntactic

import ic.doc.group15.error.SYNTACTIC_ERROR_CODE
import ic.doc.group15.error.CompilationError

abstract class SyntacticError protected constructor(
    line: Int,
    column: Int,
    message: String
) : CompilationError(SYNTACTIC_ERROR_CODE, line, column, message) {
    override fun toString(): String {
        return "Syntactic error (line $line, column $column): $message"
    }
}
