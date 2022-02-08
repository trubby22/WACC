package ic.doc.group15.semantics

import kotlin.system.exitProcess

abstract class ASTNode protected constructor(
    val parent: ASTNode?,
    val symbolTable: SymbolTable
) {
    abstract fun check()
}

class AST(topLevelSymbolTable: SymbolTable) : ASTNode(null, topLevelSymbolTable) {
    override fun check() {}
}

abstract class ExpressionAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val type: Type,
    val value: Any
) : ASTNode(parent, symbolTable)

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

    lateinit var varIdent: Variable private set

    override fun check() {
        val t = symbolTable.lookupAll(typeName)
        val v = symbolTable.lookup(varName)

        when {
            t == null -> {
                throw TypeError("unknown type $typeName")
            }
            t !is Type -> {
                throw TypeError("$typeName is not a type")
            }
            v != null -> {
                throw DeclarationError("$varName is already declared")
            }
            else -> {
                varIdent = Variable(t)
            }
        }

        symbolTable.add(varName, varIdent)
    }
}

class AssignmentAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val varname: String,
    val expr: ExpressionAST
) : ASTNode(parent, symbolTable) {

    lateinit var varIdent: Variable private set

    override fun check() {
        val v = symbolTable.lookupAll(varname)

        when {
            v == null -> {
                throw IdentifierError("unknown variable $varname")
            }
            v !is Variable -> {
                throw IdentifierError("$varname is not a variable")
            }
            v.type != expr.type -> {
                throw TypeError("$varname type not compatible with expression of type ${expr.type}")
            }
            else -> varIdent = v
        }
    }
}

class ParameterAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val typeName: String,
    val paramName: String
) : ASTNode(parent, symbolTable) {

    lateinit var paramIdent: Param private set

    public override fun check() {
        val t = symbolTable.lookupAll(typeName)
        val p = symbolTable.lookup(paramName)

        when {
            t == null -> {
                throw TypeError("unknown return type $typeName")
            }
            t !is Type -> {
                throw TypeError("$typeName is not a type")
            }
            t !is ReturnableType -> {
                throw TypeError("$typeName cannot be a parameter")
            }
            p != null -> {
                if (p is Param) {
                    throw DeclarationError(
                        "parameter $paramName already declared for this function"
                    )
                } else {
                    throw DeclarationError("identifier $paramName already declared")
                }
            }
            else -> {
                paramIdent = Param(t)
            }
        }

        symbolTable.add(paramName, paramIdent)
    }
}

class FunctionDeclarationAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val returnTypeName: String,
    val funcName: String,
    val formals: List<ParameterAST>
) : ASTNode(parent, symbolTable) {

    lateinit var funcIdent: FunctionType private set

    override fun check() {
        if (!symbolTable.isTopLevel()) {
            throw DeclarationError("functions cannot be declared in this scope")
        }

        val t = symbolTable.lookupAll(returnTypeName)
        val f = symbolTable.lookup(funcName)

        when {
            t == null -> {
                throw TypeError("unknown return type $returnTypeName")
            }
            t !is Type -> {
                throw TypeError("$returnTypeName is not a type")
            }
            t !is ReturnableType -> {
                throw TypeError("cannot return $returnTypeName type")
            }
            f != null -> {
                throw DeclarationError("function $funcName already declared")
            }
            else -> {
                val subST = symbolTable.subScope()
                for (param in formals) {
                    // TODO: set the symbol table of each param to subST, then check
                    param.check()
                }

                funcIdent = FunctionType(t, formals.map { p -> p.paramIdent }, subST)
            }
        }

        symbolTable.add(funcName, funcIdent)
    }
}

class CallAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val funcName: String,
    val actuals: List<ExpressionAST>
) : ASTNode(parent, symbolTable) {

    lateinit var funcIdent: FunctionType private set

    override fun check() {
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
                    actuals[k].check()
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

    override fun check() {
        val v = symbolTable.lookupAll(varName)

        when {
            v == null -> {
                throw IdentifierError("identifier $varName not found")
            }
            v !is Variable -> {
                throw TypeError("$varName is not a variable")
            }
            !(v is IntType || v is CharType || v is StringType || v is PairType
                    || v is ArrayType) -> {
                throw TypeError("$varName is not an int, char, string, pair " +
                        "element or array element")
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
    override fun check() {}
}

class FreeStatementAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val varName: String
) : StatementAST(parent, symbolTable) {
    val v = symbolTable.lookupAll(varName)

    override fun check() {
        when {
            v == null -> {
                throw IdentifierError("trying to free $varName, which has not" +
                        " been declared")
            }
            !(v is PairType || v is ArrayType) -> {
                throw TypeError("trying to free $varName, which is neither a " +
                        "pair nor an array")
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

    override fun check() {
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
                throw TypeError("function actual return type different from " +
                        "the one in function signature")
            }
        }
    }
}

class ExitStatementAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val expr: ExpressionAST
) : StatementAST(parent, symbolTable) {

    override fun check() {
        when {
            expr.type !is IntType -> {
                throw TypeError("expression passed to exit must be an int; " +
                        "type passed is ${expr.type}")
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
    override fun check() {
        when {
            expr.type is PairType || expr.type is ArrayType -> {
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
    override fun check() {
        when {
            condExpr.type !is BoolType -> {
                throw TypeError("type of conditional expression should be " +
                        "bool and is ${condExpr.type}")
            }
            else -> {
                thenStat.check()
                elseStat.check()
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
    override fun check() {
        when {
            condExpr.type !is BoolType -> {
                throw TypeError("type of conditional expression should be " +
                        "bool and is ${condExpr.type}")
            }
            else -> {
                stat.check()
            }
        }
    }
}

class BeginEndStatementAST(
    parent: ASTNode,
    symbolTable: SymbolTable,
    val stat: StatementAST
) : StatementAST(parent, symbolTable) {
    override fun check() {
        stat.check()
    }
}

