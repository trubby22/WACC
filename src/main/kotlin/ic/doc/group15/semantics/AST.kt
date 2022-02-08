package ic.doc.group15.semantics

import java.util.*
import kotlin.system.exitProcess

abstract class ASTNode protected constructor(
    val parent: ASTNode?,
    val symbolTable: SymbolTable
);

class AST(topLevelSymbolTable: SymbolTable) : ASTNode(null, topLevelSymbolTable);

abstract class ExpressionAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val type: Type,
    val value: Any
) : ASTNode(parent, symbolTable)

class IntExpressionAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val intExp: String,
    val intVal: Any
) : ExpressionAST(parent, symbolTable, IntType(Int.MIN_VALUE, Int.MAX_VALUE), intExp) {
    fun check() {
        if (!(intVal is IntType)) {
            throw SemanticError("Value must be of type integer\n")
        }
    }

    class BoolExpressionAST(
        parent: ASTNode,
        symbolTable: SymbolTable,
        val boolExp: String,
        val boolVal: Any
    ) : ExpressionAST(parent, symbolTable, BoolType(0, 1), boolExp) {
        fun check() {
            if (!(boolVal is BoolType)) {
                throw SemanticError("Value must be of type boolean\n")
            }
        }

        class CharExpressionAST(
            parent: ASTNode,
            symbolTable: SymbolTable,
            val charExp: String,
            val charVal: Any
        ) : ExpressionAST(parent, symbolTable, CharType(0, 255), charExp) {
            fun check() {
                if (!(charVal is CharType)) {
                    throw SemanticError("Value must be of type char\n")
                }
            }

            class StringExpressionAST(
                parent: ASTNode,
                symbolTable: SymbolTable,
                val stringExp: String,
                val stringVal: Any
            ) : ExpressionAST(parent, symbolTable, StringType(), stringExp) {
                fun check() {
                    if (!(stringVal is StringType)) {
                        throw SemanticError("Value must be of type string\n")
                    }
                }
            }

            class PairLiterExpressionAST(
                parent: ASTNode,
                symbolTable: SymbolTable,
                val pairLiterExp: String,
                val pairLiterVal: Any
            ) : ExpressionAST(parent, symbolTable, StringType(), pairLiterExp) {
                fun check() {
                    if (!(pairLiterVal is PairType)) {
                        throw SemanticError("Value must be of type pair liter\n")
                    }
                }
            }

            class VariableExpressionAST(
                parent: ASTNode,
                symbolTable: SymbolTable,
                val variableExp: String,
                val varType: Type
            ) : ExpressionAST(parent, symbolTable, varType, variableExp) {
                fun check() {
                }
            }

            class ArrayELemExpressionAST(
                parent: ASTNode,
                symbolTable: SymbolTable,
                val indexExp: ExpressionAST,
                val arrayElemExp: String,
                val arrayType: ArrayType
            ) : ExpressionAST(parent, symbolTable, arrayType, arrayElemExp) {
                fun check() {
                    //indexExp.check();
                }
            }

            class UnaryOpExpressionAST(
                parent: ASTNode,
                symbolTable: SymbolTable,
                val unOpExp: String,
                val expr: ExpressionAST,
                val unOpType: Type
            ) : ExpressionAST(parent, symbolTable, unOpType, unOpExp) {
                fun check() {
                    //expr.check()
                }
            }

            class BinaryOpExpressionAST(
                parent: ASTNode,
                symbolTable: SymbolTable,
                val binOpExp: String,
                val expr1: ExpressionAST,
                val expr2: ExpressionAST,
                val binOpType: Type
            ) : ExpressionAST(parent, symbolTable, binOpType, binOpExp) {
                fun check() {
                    //expr1.check()
                    //expr2.check()
                    if (expr1.type != expr2.type) {
                        throw TypeError("Operands must be of the same type\n")
                    }
                }
            }

            abstract class StatementAST(
                parent: ASTNode,
                symbolTable: SymbolTable
            ) : ASTNode(parent, symbolTable)

            class VariableDeclarationAST(
                parent: ASTNode,
                symbolTable: SymbolTable,
                val typeName: String,
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

            class ParameterAST(
                parent: ASTNode,
                symbolTable: SymbolTable,
                val typeName: String,
                val paramName: String
            ) : ASTNode(parent, symbolTable) {

                lateinit var paramIdent: Param
            }

            class FunctionDeclarationAST(
                parent: ASTNode,
                symbolTable: SymbolTable,
                val returnTypeName: String,
                val funcName: String,
            ) : ASTNode(parent, symbolTable) {

                val formals: MutableList<ParameterAST> = mutableListOf()

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
                            for (k in actuals.indices) {
//                    actuals[k].check()
                                if (f.formals[k].type::class != actuals[k].type::class) {
                                    throw TypeError(
                                        "type of function parameter $k incompatible with " +
                                                "declaration of $funcName"
                                    )
                                }
                            }

                            funcIdent = f
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

                    val enclosingFunction = enclosingAST as FunctionDeclarationAST
                    val t = symbolTable.lookupAll(enclosingFunction.returnTypeName)

                    when {
                        t != expr.type -> {
                            throw TypeError(
                                "function actual return type different from " +
                                        "the one in function signature"
                            )
                        }
                    }
                }
            }

            class ExitStatementAST(
                parent: ASTNode,
                symbolTable: SymbolTable,
                val expr: ExpressionAST
            ) : StatementAST(parent, symbolTable) {

                fun check() {
                    when (expr.type) {
                        !is IntType -> {
                            throw TypeError(
                                "expression passed to exit must be an int; " +
                                        "type passed is ${expr.type}"
                            )
                        }
                        else -> {
                            exitProcess(expr.value as Int)
                        }
                    }
                }
            }

            class PrintStatementAST(
                parent: ASTNode,
                symbolTable: SymbolTable,
                val expr: ExpressionAST
            ) : StatementAST(parent, symbolTable) {
                fun check() {
                    when (expr.type) {
                        is PairType, is ArrayType -> {
                            throw TypeError("trying to print illegal type: ${expr.type}")
                        }
                    }
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
                    when (condExpr.type) {
                        !is BoolType -> {
                            throw TypeError(
                                "type of conditional expression should be " +
                                        "bool and is ${condExpr.type}"
                            )
                        }
                        else -> {
//                thenStat.check()
//                elseStat.check()
                        }
                    }
                }
            }

            class WhileStatementAST(
                parent: ASTNode,
                symbolTable: SymbolTable,
                val condExpr: ExpressionAST,
                val stat: StatementAST
            ) : StatementAST(parent, symbolTable) {
                fun check() {
                    when (condExpr.type) {
                        !is BoolType -> {
                            throw TypeError(
                                "type of conditional expression should be " +
                                        "bool and is ${condExpr.type}"
                            )
                        }
                        else -> {
//                stat.check()
                        }
                    }
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
        }
    }
}
