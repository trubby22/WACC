package ic.doc.group15.visitor

import ic.doc.group15.ast.BlockAST

abstract class ArrayUse(
    open val scope: BlockAST
)

class ArrayDeclaration(
    override val scope: BlockAST,
    val references: MutableMap<Int, String> = mutableMapOf()
): ArrayUse(scope)

class ArrayAccess(
    override val scope: BlockAST,
    val index: Int
): ArrayUse(scope)

class ArrayFree(
    override val scope: BlockAST
): ArrayUse(scope)
