package ic.doc.group15.visitor

import ic.doc.group15.assembly.TranslatorMethod
import ic.doc.group15.ast.*
import java.util.*
import javax.lang.model.type.ArrayType

class BCEOptimizerSeq (
    private val ast: AST,
    private val enableLogging: Boolean = true
) : ASTVisitor(ast, enableLogging) {

    private val arrayUses: MutableMap<String, LinkedList<ArrayUse>> =
        mutableMapOf()
    private val functionDeclarations: MutableMap<String,
            FunctionDeclarationAST> = mutableMapOf()

    @TranslatorMethod(AST::class)
    private fun translateProgram(node: AST): AST {
        log("Translating ${node.javaClass}")

        val functionASTs = node.statements.filterIsInstance<FunctionDeclarationAST>()
        functionASTs.forEach { translate(it) }

        val statementASTs = node.statements.filter { it !is FunctionDeclarationAST }
        statementASTs.forEach { translate(it) }

        return node
    }

    @TranslatorMethod(FunctionDeclarationAST::class)
    private fun translateFunctionDeclaration(node: FunctionDeclarationAST):
            FunctionDeclarationAST {
        log("Translating ${node.javaClass}")
        functionDeclarations[node.funcName] = node
        return node
    }

    @TranslatorMethod(VariableDeclarationAST::class)
    private fun translateVariableDeclaration(node: VariableDeclarationAST):
            VariableDeclarationAST {
        log("Translating ${node.javaClass}")
        if (node.varIdent.type is ArrayType) {
            addArrayUse(node.varName, ArrayDeclaration(node.parent!!))
        }
        return node
    }

    @TranslatorMethod(ReadStatementAST::class)
    private fun translateRead(node: ReadStatementAST): ReadStatementAST {
        log("Translating ${node.javaClass}")
        translate(node.target)
        return node
    }

    @TranslatorMethod(SkipStatementAST::class)
    private fun translateSkip(node: SkipStatementAST): SkipStatementAST {
        log("Translating ${node.javaClass}")
        return node
    }

    @TranslatorMethod(FreeStatementAST::class)
    private fun translateFree(node: FreeStatementAST): FreeStatementAST {
        log("Translating ${node.javaClass}")
        if (node.expr.type is ArrayType) {
            if (node.expr is VariableIdentifierAST) {
                addArrayUse(node.expr.varName, ArrayFree(node.parent!!))
            } else if (node.expr is ArrayElemAST) {
                freeArrayAtIndex(node.expr.arrayVar.varName, node.expr.indexExpr
                    .map{ evalExpr(it) }, node)
            } else {
                throw Error("Shouldn't get here")
            }
        }
        return node
    }

    @TranslatorMethod(ReturnStatementAST::class)
    private fun translateReturn(node: ReturnStatementAST): ReturnStatementAST {
        log("Translating ${node.javaClass}")
        translate(node.expr)
        return node
    }

    @TranslatorMethod(ExitStatementAST::class)
    private fun translateExit(node: ExitStatementAST): ExitStatementAST {
        log("Translating ${node.javaClass}")
        translate(node.expr)
        return node
    }

    @TranslatorMethod(PrintStatementAST::class)
    private fun translatePrint(node: PrintStatementAST): PrintStatementAST {
        log("Translating ${node.javaClass}")
        translate(node.expr)
        return node
    }

    @TranslatorMethod(PrintlnStatementAST::class)
    private fun translatePrintln(node: PrintlnStatementAST): PrintlnStatementAST {
        log("Translating ${node.javaClass}")
        translate(node.expr)
        return node
    }

    @TranslatorMethod(ArrayElemAST::class)
    private fun translateArrayElem(node: ArrayElemAST): ArrayElemAST {
        log("Translating ${node.javaClass}")
        val name = node.arrayVar.varName
        val indices = node.indexExpr.map { evalExpr(it) }
        tryEliminatingBoundsCheck(name, indices, node)
        accessArrayAtIndex(name, indices, node)
        return node
    }

    @TranslatorMethod(BeginEndBlockAST::class)
    private fun translateBeginEnd(node: BeginEndBlockAST): BeginEndBlockAST {
        log("Translating ${node.javaClass}")
        return node
    }

    @TranslatorMethod(IfBlockAST::class)
    private fun translateIfBlock(node: IfBlockAST): IfBlockAST {
        log("Translating ${node.javaClass}")
        return node
    }

    @TranslatorMethod(ElseBlockAST::class)
    private fun translateElseBlock(node: ElseBlockAST): ElseBlockAST {
        log("Translating ${node.javaClass}")
        return node
    }

    @TranslatorMethod(WhileBlockAST::class)
    private fun translateWhile(node: WhileBlockAST): WhileBlockAST {
        log("Translating ${node.javaClass}")
        return node
    }



//    endregion

    private fun addArrayUse(name: String, arrayUse: ArrayUse) {
        val stack: LinkedList<ArrayUse>? = arrayUses[name]
        if (stack == null) {
            val newStack: LinkedList<ArrayUse> = LinkedList()
            newStack.push(arrayUse)
            arrayUses[name] = newStack
        } else {
            stack.push(arrayUse)
        }
    }

    private fun freeArrayAtIndex(
        name: String,
        indices: List<Int?>,
        scope: ASTNode
    ) {
        val first = indices.first()
        if (indices.isEmpty() || first == null) {
            addArrayUse(name, ArrayFree(scope))
        } else {
            freeArrayAtIndex(
                getLatestArrayDeclaration(name).references[first]!!,
                indices.subList(1, indices.size),
                scope
            )
        }
    }

    private fun accessArrayAtIndex(
        name: String,
        indices: List<Int?>,
        scope: ASTNode
    ) {
        val first = indices.first()
        if (indices.isNotEmpty() && first != null) {
            addArrayUse(name, ArrayAccess(scope, first))
            accessArrayAtIndex(
                getLatestArrayDeclaration(name).references[first]!!,
                indices.subList(1, indices.size),
                scope
            )
        }
    }

    private fun tryEliminatingBoundsCheck(
        name: String,
        indices: List<Int?>,
        node: ArrayElemAST
    ) {
        TODO("Not yet implemented")
    }

    private fun getLatestArrayDeclaration(name: String):
            ArrayDeclaration {
        return arrayUses[name]?.dropWhile { it !is ArrayDeclaration }?.first() as ArrayDeclaration
    }

    private fun evalExpr(node: ExpressionAST): Int? {
        return if (node is IntLiteralAST) {
            node.intValue
        } else {
            null
        }
    }

}