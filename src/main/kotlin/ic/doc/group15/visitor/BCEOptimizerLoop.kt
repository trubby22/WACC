package ic.doc.group15.visitor

import ic.doc.group15.ast.AST

class BCEOptimizerLoop(
    private val ast: AST,
    private val enableLogging: Boolean = true
) : ASTVisitor(ast, enableLogging) {

}