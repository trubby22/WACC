package ic.doc.group15.semantics

import java.util.*
import kotlin.system.exitProcess

abstract class ASTNode protected constructor(
    val parent: ASTNode?,
    val symbolTable: SymbolTable = SymbolTable.emptyTable
)

class AST(topLevelSymbolTable: SymbolTable) : ASTNode(null, topLevelSymbolTable) {
    val functions: MutableList<FunctionDeclarationAST> = LinkedList()
    lateinit var main: StatementAST
}

abstract class ExpressionAST protected constructor(
    parent: ASTNode,
    symbolTable: SymbolTable = SymbolTable.emptyTable
) : ASTNode(parent, symbolTable)

class IntLiteralAST(
    parent: ASTNode,
    val intValue: Int
) : ExpressionAST(parent)

class BoolLiteralAST(
    parent: ASTNode,
    val boolValue: Int
) : ExpressionAST(parent)

class CharLiteralAST(
    parent: ASTNode,
    val charValue: Char
) : ExpressionAST(parent)

class StringLiteralAST(
    parent: ASTNode,
    val stringValue: String
) : ExpressionAST(parent)

class NullPairLiteralAST(
    parent: ASTNode
) : ExpressionAST(parent)

class VariableExpressionAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val varName: String
) : ExpressionAST(parent, symbolTable) {

    lateinit var varIdent: Variable
}

class UnaryOpAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val unOpExp: String,
    val expr: ExpressionAST,
    val unOpType: Type
) : ExpressionAST(parent, symbolTable) {
    fun check() {
        //expr.check()
    }
}

class BinaryOpAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val binOpExp: String,
    val expr1: ExpressionAST,
    val expr2: ExpressionAST,
    val binOpType: Type
) : ExpressionAST(parent, symbolTable) {
    fun check() {
        //expr1.check()
        //expr2.check()
//        if (expr1.type != expr2.type) {
//            throw TypeError("Operands must be of the same type\n")
//        }
    }
}

abstract class StatementAST protected constructor(
    parent: ASTNode,
    symbolTable: SymbolTable
) : ASTNode(parent, symbolTable)

open class VariableDeclarationAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    open val typeName: String,
    val varName: String
) : ASTNode(parent, symbolTable) {

    lateinit var varIdent: Variable
}

class VariableAssignmentAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val varname: String,
    val expr: ExpressionAST
) : ASTNode(parent, symbolTable) {

    lateinit var varIdent: Variable
}

class ArrayElemAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val indexExp: ExpressionAST,
    val arrayName: String,
    val arrayType: ArrayType
) : ASTNode(parent, symbolTable)

class ParameterAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    override val typeName: String,
    val paramName: String
) : VariableDeclarationAST(parent, symbolTable, typeName, paramName) {

    lateinit var paramIdent: Param
}

class FunctionDeclarationAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val returnTypeName: String,
    val funcName: String,
) : ASTNode(parent, symbolTable) {

    val formals: MutableList<ParameterAST> = mutableListOf()

    lateinit var body: StatementAST

    lateinit var funcIdent: FunctionType
}

class CallAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val funcName: String,
    val actuals: List<ExpressionAST>
) : ASTNode(parent, symbolTable) {

    lateinit var funcIdent: FunctionType private set

    fun check() {
        val f = symbolTable.lookupAll(funcName)

        when {
            f == null -> {
                throw IdentifierError("function $funcName not found")
            }
            f !is FunctionType -> {
                throw TypeError("$funcName is not a function")
            }
            f.formals.size != actuals.size -> {
                throw ParameterError(
                    "wrong number of arguments for $funcName: " +
                            "expected ${f.formals.size}, got ${actuals.size}"
                )
            }
            else -> {
//                for (k in actuals.indices) {
////                    actuals[k].check()
//                    if (f.formals[k].type::class != actuals[k].type::class) {
//                        throw TypeError(
//                            "type of function parameter $k incompatible with " +
//                                    "declaration of $funcName"
//                        )
//                    }
//                }
//
//                funcIdent = f
            }
        }
    }
}

class ReadStatementAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val varName: String
) : StatementAST(parent, symbolTable) {

    lateinit var varIdent: Variable private set

    fun check() {
        val v = symbolTable.lookupAll(varName)

        when {
            v == null -> {
                throw IdentifierError("identifier $varName not found")
            }
            v !is Variable -> {
                throw TypeError("$varName is not a variable")
            }
            !(
                    v is IntType || v is CharType || v is StringType || v is PairType ||
                            v is ArrayType
                    ) -> {
                throw TypeError(
                    "$varName is not an int, char, string, pair " +
                            "element or array element"
                )
            }
            else -> {
                varIdent = v
            }
        }

        symbolTable.add(varName, varIdent)
    }
}

class SkipStatementAST(
    parent: ASTNode,
    symbolTable: SymbolTable
) : StatementAST(parent, symbolTable) {
    fun check() {}
}

class FreeStatementAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val varName: String
) : StatementAST(parent, symbolTable) {
    val v = symbolTable.lookupAll(varName)

    fun check() {
        when {
            v == null -> {
                throw IdentifierError(
                    "trying to free $varName, which has not" +
                            " been declared"
                )
            }
            !(v is PairType || v is ArrayType) -> {
                throw TypeError(
                    "trying to free $varName, which is neither a " +
                            "pair nor an array"
                )
            }
        }

        symbolTable.remove(varName)
    }
}

class ReturnStatementAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val expr: ExpressionAST
) : StatementAST(parent, symbolTable) {

    fun check() {
        var enclosingAST = parent

        while (enclosingAST != null && enclosingAST !is FunctionDeclarationAST) {
            enclosingAST = enclosingAST.parent
        }

        if (enclosingAST == null) {
            throw SemanticError("could not find enclosing function")
        }

//        val enclosingFunction = enclosingAST as FunctionDeclarationAST
//        val t = symbolTable.lookupAll(enclosingFunction.returnTypeName)

//        when {
//            t != expr.type -> {
//                throw TypeError(
//                    "function actual return type different from " +
//                            "the one in function signature"
//                )
//            }
//        }
    }
}

class ExitStatementAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val expr: ExpressionAST
) : StatementAST(parent, symbolTable) {

//    fun check() {
//        when (expr.type) {
//            !is IntType -> {
//                throw TypeError(
//                    "expression passed to exit must be an int; " +
//                            "type passed is ${expr.type}"
//                )
//            }
//            else -> {
//                exitProcess(expr.value as Int)
//            }
//        }
//    }
}

class PrintStatementAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val expr: ExpressionAST
) : StatementAST(parent, symbolTable) {
    fun check() {
//        when (expr.type) {
//            is PairType, is ArrayType -> {
//                throw TypeError("trying to print illegal type: ${expr.type}")
//            }
//        }
    }
}

class IfStatementAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val condExpr: ExpressionAST,
    val thenStat: StatementAST,
    val elseStat: StatementAST
) : StatementAST(parent, symbolTable) {
    fun check() {
//        when (condExpr.type) {
//            !is BoolType -> {
//                throw TypeError(
//                    "type of conditional expression should be " +
//                            "bool and is ${condExpr.type}"
//                )
//            }
//            else -> {
////                thenStat.check()
////                elseStat.check()
//            }
//        }
    }
}

class WhileStatementAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val condExpr: ExpressionAST,
    val stat: StatementAST
) : StatementAST(parent, symbolTable) {
    fun check() {
//        when (condExpr.type) {
//            !is BoolType -> {
//                throw TypeError(
//                    "type of conditional expression should be " +
//                            "bool and is ${condExpr.type}"
//                )
//            }
//            else -> {
////                stat.check()
//            }
//        }
    }
}

class BeginEndStatementAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val stat: StatementAST
) : StatementAST(parent, symbolTable) {
    fun check() {
//        stat.check()
    }
}
