package ic.doc.group15.error

import org.antlr.v4.runtime.BaseErrorListener
import org.antlr.v4.runtime.RecognitionException
import org.antlr.v4.runtime.Recognizer
import kotlin.system.exitProcess

class SyntacticErrorListener : BaseErrorListener() {
    private var errors = 0

    override fun syntaxError(
        recognizer: Recognizer<*, *>?,
        offendingSymbol: Any?,
        line: Int,
        charPositionInLine: Int,
        msg: String?,
        e: RecognitionException?
    ) {
        errors ++
    }

    fun terminateIfSyntacticErrorOccurred() {
        if (errors > 0) {
            exitProcess(100)
        }
    }
}
