package ic.doc.group15.semantics

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
    val type: Type
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
