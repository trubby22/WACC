package ic.doc.group15.visitor

import ic.doc.group15.ast.AST
import ic.doc.group15.error.SyntacticErrorList

class ReturnChecker(
    private val ast: AST,
    private val syntacticErrors: SyntacticErrorList,
    private val enableLogging: Boolean = false
) {


}