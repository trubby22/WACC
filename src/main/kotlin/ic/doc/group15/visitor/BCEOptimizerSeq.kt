package ic.doc.group15.visitor

import ic.doc.group15.assembly.TranslatorMethod
import ic.doc.group15.ast.*
import ic.doc.group15.error.BCEErrorList
import ic.doc.group15.error.optimization.CannotEvaluateIndicesError
import ic.doc.group15.type.ArrayType
import ic.doc.group15.type.Type
import java.util.*

class BCEOptimizerSeq (
    private val ast: AST,
    private val errors: BCEErrorList,
    private val enableLogging: Boolean = true
) : ASTVisitor(ast, enableLogging) {

    private val arrayUses: MutableMap<String, LinkedList<ArrayUse>> =
        mutableMapOf()
    private val functionDeclarations: MutableMap<String,
            FunctionDeclarationAST> = mutableMapOf()

    fun removeArrayBoundChecking() {
        log("----------------")
        log("Begin BCE optimization")
        translateProgram(ast)
        log("BCE optimization complete")
        log("----------------")
    }

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
        node.formals.forEach { translate(it, node) }
        node.statements.forEach { translate(it) }
        removeScopeArrayUses(node)
        return node
    }

    @TranslatorMethod(VariableDeclarationAST::class)
    private fun translateVariableDeclaration(node: VariableDeclarationAST):
            VariableDeclarationAST {
        log("Translating ${node.javaClass}")
        assignToArray(node.parent!!, node.rhs, node.varIdent.type, node.varName)
        return node
    }

    @TranslatorMethod(AssignToIdentAST::class)
    private fun translateAssignToIdent(node: AssignToIdentAST): AssignToIdentAST {
        log("Translating ${node.javaClass}")
        translate(node.rhs, node.parent!!)
        translate(node.lhs)
        assignToArray(node.parent, node.rhs, node.type, node.lhs.varName)
        return node
    }

    @TranslatorMethod(AssignToArrayElemAST::class)
    private fun translateAssignToArrayElem(node: AssignToArrayElemAST):
            AssignToArrayElemAST {
        log("Translating ${node.javaClass}")
        translate(node.rhs, node.parent!!)
        translate(node.lhs)
        assignToArray(
            node.parent,
            node.rhs,
            node.type,
            getArrayName(
                node.lhs.arrayVar.varName,
                node.lhs.indexExpr.map { evalExpr(it) },
                node.parent)
        )
        return node
    }

    @TranslatorMethod(AssignToPairElemAST::class)
    private fun translateAssignToPairElem(node: AssignToPairElemAST):
            AssignToPairElemAST {
        log("Translating ${node.javaClass}")
        translate(node.rhs, node.parent!!)
        return node
    }

    @TranslatorMethod(ReadStatementAST::class)
    private fun translateRead(node: ReadStatementAST): ReadStatementAST {
        log("Translating ${node.javaClass}")
        translate(node.target, node.parent!!)
        return node
    }

    @TranslatorMethod(FreeStatementAST::class)
    private fun translateFree(node: FreeStatementAST): FreeStatementAST {
        log("Translating ${node.javaClass}")
        if (node.expr.type is ArrayType) {
            if (node.expr is VariableIdentifierAST) {
                addArrayUse(node.expr.varName, ArrayFree(node.parent!!))
            } else if (node.expr is ArrayElemAST) {
                addArrayUse(getArrayName(
                    node.expr.arrayVar.varName,
                    node.expr.indexExpr.map{ evalExpr(it) },
                    node.parent!!),
                ArrayFree(node.parent))
            } else {
                throw Error("Shouldn't get here")
            }
        }
        return node
    }

    @TranslatorMethod(ReturnStatementAST::class)
    private fun translateReturn(node: ReturnStatementAST): ReturnStatementAST {
        log("Translating ${node.javaClass}")
        translate(node.expr, node.parent!!)
        return node
    }

    @TranslatorMethod(ExitStatementAST::class)
    private fun translateExit(node: ExitStatementAST): ExitStatementAST {
        log("Translating ${node.javaClass}")
        translate(node.expr, node.parent!!)
        return node
    }

    @TranslatorMethod(PrintStatementAST::class)
    private fun translatePrint(node: PrintStatementAST): PrintStatementAST {
        log("Translating ${node.javaClass}")
        translate(node.expr, node.parent!!)
        return node
    }

    @TranslatorMethod(PrintlnStatementAST::class)
    private fun translatePrintln(node: PrintlnStatementAST): PrintlnStatementAST {
        log("Translating ${node.javaClass}")
        translate(node.expr, node.parent!!)
        return node
    }

    @TranslatorMethod(UnaryOpExprAST::class)
    private fun translateUnaryExpr(node: UnaryOpExprAST, scope: BlockAST):
            UnaryOpExprAST {
        log("Translating ${node.javaClass}")
        translate(node.expr, scope)
        return node
    }

    @TranslatorMethod(BinaryOpExprAST::class)
    private fun translateBinaryExpr(node: BinaryOpExprAST, scope: BlockAST):
            BinaryOpExprAST {
        log("Translating ${node.javaClass}")
        translate(node.expr1, scope)
        translate(node.expr2, scope)
        return node
    }

    @TranslatorMethod(ArrayElemAST::class)
    private fun translateArrayElem(node: ArrayElemAST, scope: BlockAST):
            ArrayElemAST {
        log("Translating ${node.javaClass}")
        val name = node.arrayVar.varName
        val indices = node.indexExpr.map { evalExpr(it) }
        listEliminateBC(name, indices, node)
        accessArrayAtIndex(name, indices, scope)
        return node
    }

    @TranslatorMethod(BeginEndBlockAST::class)
    private fun translateBeginEnd(node: BeginEndBlockAST): BeginEndBlockAST {
        log("Translating ${node.javaClass}")
        node.statements.forEach { translate(it) }
        removeScopeArrayUses(node)
        return node
    }

    @TranslatorMethod(IfBlockAST::class)
    private fun translateIfBlock(node: IfBlockAST): IfBlockAST {
        log("Translating ${node.javaClass}")
        translate(node.condExpr, node)
        node.statements.forEach { translate(it) }
        removeScopeArrayUses(node)
        removeScopeArrayAccesses(node)
        return node
    }

    @TranslatorMethod(ElseBlockAST::class)
    private fun translateElseBlock(node: ElseBlockAST): ElseBlockAST {
        log("Translating ${node.javaClass}")
        node.statements.forEach { translate(it) }
        removeScopeArrayUses(node)
        removeScopeArrayAccesses(node)
        return node
    }

    @TranslatorMethod(WhileBlockAST::class)
    private fun translateWhile(node: WhileBlockAST): WhileBlockAST {
        log("Translating ${node.javaClass}")
        translate(node.condExpr, node)
        node.statements.forEach { translate(it) }
        removeScopeArrayUses(node)
        removeScopeArrayAccesses(node)
        return node
    }

    @TranslatorMethod(CallAST::class)
    private fun translateCall(node: CallAST, scope: BlockAST): CallAST {
        log("Translating ${node.javaClass}")
        val arrayParams = node.actuals.filter { it.type is ArrayType }
        arrayParams.forEach { translate(it, scope) }
        arrayParams.filterIsInstance<VariableIdentifierAST>()
            .flatMap { getAllReferences((it as VariableIdentifierAST).varName) }
            .forEach { addArrayUse(it, ArrayFree(scope)) }
        arrayParams.filterIsInstance<ArrayElemAST>()
            .forEach {
                addArrayUse(getArrayName(
                    (it as ArrayElemAST).arrayVar.varName,
                    it.indexExpr.map { evalExpr(it) },
                    scope
                    ), ArrayFree(scope))
            }
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

    private fun accessArrayAtIndex(
        name: String,
        indices: List<Int?>,
        scope: BlockAST
    ) {
        val first = indices.first()
        if (indices.isNotEmpty() && first != null) {
            addArrayUse(name, ArrayAccess(scope, first))
            if (indices.size > 1) {
                accessArrayAtIndex(
                    getLatestArrayDeclaration(name).references[first]!!,
                    indices.subList(1, indices.size),
                    scope
                )
            }
        }
    }

    private fun getArrayName(
        name: String,
        indices: List<Int?>,
        scope: BlockAST
    ) : String {
        val first = indices.first()
        return if (indices.isEmpty()) {
            name
        } else if (first == null) {
            errors.addError(CannotEvaluateIndicesError())
            "error"
        } else {
            getArrayName(
                getLatestArrayDeclaration(name).references[first]!!,
                indices.subList(1, indices.size),
                scope
            )
        }
    }

    private fun assignToArray(
        scope: BlockAST,
        rhs: AssignRhsAST,
        lhsType: Type,
        name: String
    ) {
        if (lhsType is ArrayType) {
            var references = mutableMapOf<Int, String>()
            if (rhs is ArrayLiteralAST) {
                rhs.elems.forEachIndexed { index, element ->
                    if (element.type is ArrayType) {
                        if (element is VariableIdentifierAST) {
                            references[index] = element.varName
                        } else if (element is ArrayElemAST) {
                            references[index] = getArrayName(
                                element.arrayVar.varName,
                                element.indexExpr.map { evalExpr(it) },
                                scope
                            )
                        }
                    }
                }
            } else if (rhs is VariableIdentifierAST) {
                references = getLatestArrayDeclaration(rhs.varName).references
            }
            addArrayUse(name, ArrayDeclaration(scope, references))
        }
    }

    private fun listEliminateBC(
        name: String,
        indices: List<Int?>,
        node: ArrayElemAST
    ) {
        val possibleBCEs = mutableListOf<Boolean>()
        var currentName = name
        val validIndices = indices.takeWhile { it != null }
        for (i in validIndices.indices) {
            val index = validIndices[i]
            possibleBCEs.add(eliminateBC(currentName, index!!))
            if (i == validIndices.size - 1) {
                break
            }
            currentName = getLatestArrayDeclaration(currentName)
                .references[index]!!
        }
        indices.dropWhile { it != null }.forEach { possibleBCEs.add(false) }
        node.noBoundCheckRequired = possibleBCEs
    }

    private fun eliminateBC(name: String, index: Int): Boolean {
        if (arrayUses[name]!!.filterIsInstance<ArrayFree>().isNotEmpty()) {
            return false
        }
        val max = arrayUses[name]!!.takeWhile { it is ArrayAccess }.map { (it as
                ArrayAccess).index }.maxOrNull() ?: return false
        return index <= max
    }

    private fun removeScopeArrayUses(scope: BlockAST) {
        for (key in arrayUses.keys) {
            arrayUses[key] = arrayUses[key]!!
                .takeLastWhile { !(it is ArrayDeclaration && it.scope == scope) }
                    as LinkedList<ArrayUse>
        }
    }

    private fun removeScopeArrayAccesses(scope: BlockAST) {
        for (key in arrayUses.keys) {
            arrayUses[key] = arrayUses[key]!!
                .filter { !(it is ArrayAccess && isSubscopeOf(it.scope, 
                    scope)) }
                    as LinkedList<ArrayUse>
        }
    }

    private fun isSubscopeOf(subscope: BlockAST, superscope: BlockAST): 
            Boolean {
        if (subscope == superscope) {
            return true
        }
        if (subscope.parent == null) {
            return false
        }
        return isSubscopeOf(subscope.parent, superscope)
    }

    private fun getLatestArrayDeclaration(name: String):
            ArrayDeclaration {
        return arrayUses[name]?.dropWhile { it !is ArrayDeclaration }?.first() as ArrayDeclaration
    }

    private fun getAllReferences(name: String): Set<String> {
        return setOf(name) + getLatestArrayDeclaration(name).references.values
            .flatMap {
            getAllReferences(it) }.toSet()
    }

    private fun evalExpr(node: ExpressionAST): Int? {
        return if (node is IntLiteralAST) {
            node.intValue
        } else {
            null
        }
    }

}