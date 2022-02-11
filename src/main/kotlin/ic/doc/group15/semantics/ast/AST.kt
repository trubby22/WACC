package ic.doc.group15.semantics.ast

import ic.doc.group15.semantics.SymbolTable

abstract class ASTNode protected constructor(
    val symbolTable: SymbolTable = SymbolTable.emptyTable
)

class AST(topLevelSymbolTable: SymbolTable) : BeginEndBlockAST(null, topLevelSymbolTable)
