package ic.doc.group15.error.syntactic

import ic.doc.group15.error.SYNTACTIC_ERROR_CODE
import ic.doc.group15.error.WaccError
import org.antlr.v4.runtime.Token

open class SyntacticError(
    line: Int,
    column: Int,
    message: String
) : WaccError(SYNTACTIC_ERROR_CODE, line, column, message) {
    override fun toString(): String {
        return "Syntactic error (line $line, column $column): $message"
    }
}

class MissingReturnError(
    token: Token,
    funcName: String
) : SyntacticError(
    token.line,
    token.charPositionInLine,
    "Function $funcName is not ended with a return or an exit statement."
)
