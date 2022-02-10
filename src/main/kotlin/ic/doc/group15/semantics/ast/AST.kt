package ic.doc.group15.semantics

import ic.doc.group15.semantics.ast.BeginEndBlockAST

abstract class ASTNode protected constructor(
    val symbolTable: SymbolTable = SymbolTable.emptyTable
)

class AST(topLevelSymbolTable: SymbolTable) : BeginEndBlockAST(null, topLevelSymbolTable)
