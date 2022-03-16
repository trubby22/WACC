package ic.doc.group15.ast

import ic.doc.group15.SymbolTable
import java.util.*

abstract class ASTNode protected constructor(
    val symbolTable: SymbolTable = SymbolTable.emptyTable
)

class AST(topLevelSymbolTable: SymbolTable) : BlockAST(null, topLevelSymbolTable) {

    private val funcs: MutableList<FunctionDeclarationAST> = LinkedList()

    private val main: MutableList<StatementAST> = LinkedList()

    override fun addStatement(stat: StatementAST): StatementAST {
        if (stat is FunctionDeclarationAST) {
            funcs.add(stat)
        } else {
            main.add(stat)
        }
        return stat
    }

    fun getFuncs(): List<FunctionDeclarationAST> = funcs

    fun getMain(): List<StatementAST> = main
}
