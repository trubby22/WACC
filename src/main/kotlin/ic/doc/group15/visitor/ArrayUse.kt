package ic.doc.group15.visitor

import ic.doc.group15.ast.ASTNode

abstract class ArrayUse(
    open val scope: ASTNode
)

class ArrayDeclaration(
    override val scope: ASTNode,
    val references: MutableMap<Int, String> = mutableMapOf()
): ArrayUse(scope)

class ArrayAccess(
    override val scope: ASTNode,
    val index: Int
): ArrayUse(scope)

class ArrayFree(
    override val scope: ASTNode,
    val index: Int? = null
): ArrayUse(scope)
