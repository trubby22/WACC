package ic.doc.group15.error

import org.antlr.v4.runtime.Token

const val SYNTACTIC_ERROR_CODE = 100
const val SEMANTIC_ERROR_CODE = 200

abstract class WaccError protected constructor(
    val errorCode: Int,
    val line: Int,
    val column: Int,
    message: String
) : Throwable(message) {

    protected constructor(
        errorCode: Int,
        token: Token,
        message: String
    ) : this(
        errorCode,
        token.line,
        token.charPositionInLine, message
    )
}
