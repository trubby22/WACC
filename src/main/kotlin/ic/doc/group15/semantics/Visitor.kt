package ic.doc.group15.semantics

import ic.doc.group15.antlr.WaccParser
import ic.doc.group15.antlr.WaccParserBaseVisitor
import java.util.logging.Level
import java.util.logging.Logger

class Visitor(
    private val abstractSyntaxTree: AST,
    private val symbolTable: SymbolTable
) : WaccParserBaseVisitor<ASTNode>() {

    private var scope: BlockAST = abstractSyntaxTree
    private var symbols = symbolTable

    companion object {
        private val LOG = Logger.getLogger(Visitor::class.java.name)

        private fun log(message: String) {
            LOG.log(Level.FINE, message.trimMargin())
        }
    }

    override fun visitProgram(ctx: WaccParser.ProgramContext): ASTNode {
        assert(symbols.isTopLevel())

        log("Begin program semantic analysis")
        val program = scope as AST

        log("Visiting function definitions")
        for (func in ctx.func()) {
            program.statements.add(visitFunc(func) as FunctionDeclarationAST)
        }

        log("Visiting main program")
        visit(ctx.stat()) as StatementAST

        return program
    }

    override fun visitFunc(ctx: WaccParser.FuncContext): ASTNode {
        val funcName = ctx.ident().text
        val returnTypeName = ctx.type().text

        log(
            """Visiting function declaration
                || Function name: $funcName
                || Return type: $returnTypeName
            """
        )

        if (!symbols.isTopLevel()) {
            throw DeclarationError("functions cannot be declared in this scope")
        }

        val t = symbols.lookupAll(returnTypeName)
        val f = symbols.lookup(funcName)

        val func: FunctionDeclarationAST

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
                func = FunctionDeclarationAST(
                    scope, symbols, returnTypeName, funcName
                )

                log(
                    """Visiting parameters of function ${func.funcName}"""
                )

                symbols = symbols.subScope()
                scope = func

                for (param in ctx.param()) {
                    func.formals.add(visitParam(param) as ParameterAST)
                }

                symbols = symbols.parentScope()!!
                scope = scope.parent!! as BlockAST

                func.funcIdent = FunctionType(
                    t,
                    func.formals.map { p -> p.paramIdent },
                    symbols
                )

                symbolTable.add(funcName, func.funcIdent)

                val body = visit(ctx.stat()) as StatementAST
                func.body = body

                if (func.returnStat == null) {
                    throw DeclarationError(
                        "function $funcName missing return statement"
                    )
                }
            }
        }

        return func
    }

    override fun visitParam(ctx: WaccParser.ParamContext): ASTNode? {
        val typeName = ctx.type().text
        val paramName = ctx.ident().text

        log(
            """Visiting parameter
                || Type name: $typeName
                || Param name: $paramName
            """
        )

        val parameterAST = ParameterAST(scope, symbols, typeName, paramName)

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
                parameterAST.paramIdent = Param(t)
            }
        }

        symbolTable.add(paramName, parameterAST.paramIdent)

        return parameterAST
    }

    override fun visitDeclarationStat(ctx: WaccParser.DeclarationStatContext): ASTNode? {

        val typeName = ctx.type().text
        val varName = ctx.ident().text

        log(
            """Visiting variable declaration 
                || Type name: $typeName
                || Var name: $varName
            """
        )

        val varDecl = VariableDeclarationAST(scope, symbols, typeName, varName)

        val t = symbols.lookupAll(typeName)
        val v = symbols.lookup(varName)

        when {
            t == null -> {
                throw TypeError("unknown type $typeName")
            }
            t !is Type -> {
                throw TypeError("$typeName is not a type")
            }
            v != null -> {
                throw DeclarationError("$varName has already been declared")
            }
            else -> {
                varDecl.varIdent = Variable(t)
            }
        }

        symbols.add(varName, varDecl.varIdent)

        return varDecl
    }

    override fun visitAssignmentStat(ctx: WaccParser.AssignmentStatContext): ASTNode? {

        val varName = ctx.ident().text
        val expr = ctx.expr()
        val exprName = expr.text

        log(
            """Visiting variable assignment
                || Identifier: $varName
                || Expression: $exprName
            """.trimIndent()
        )

        val varAssign = VariableAssignmentAST(ast, st, varName, exprName)

        // identifier may be declared in parent scope
        val v = st.lookupAll(varName)

        when {
            v == null -> {
                throw DeclarationError("$varName has not been declared")
            }
            v !is Variable -> {
                throw IdentifierError("$varName is not a variable")
            }
            v.type != expr.type -> {
                throw TypeError("$varName type(${v.type}) not compatible with expression type (${expr.type})")
            }
            else -> {
                varAssign.varIdent = expr
            }
        }

        st.add(varName, varAssign.varIdent)

        return varAssign
    }

    override fun visitUnaryOpExpr(ctx: WaccParser.UnaryOpExprContext?): ASTNode {
        return super.visitUnaryOpExpr(ctx)
    }

    override fun visitBinaryOpExpr(ctx: WaccParser.BinaryOpExprContext?): ASTNode {
        return super.visitBinaryOpExpr(ctx)
    }

    override fun visitParenExpr(ctx: WaccParser.ParenExprContext?): ASTNode {
        return super.visitParenExpr(ctx)
    }


    override fun visitInt_liter(ctx: WaccParser.Int_literContext?): ASTNode {
        return super.visitInt_liter(ctx)
    }

    override fun visitTBool(ctx: WaccParser.TBoolContext?): ASTNode {
        return BoolLiteralAST(ast, 1)
    }

    override fun visitFBool(ctx: WaccParser.FBoolContext?): ASTNode {
        return BoolLiteralAST(ast, 0)
    }

    override fun visitChar_liter(ctx: WaccParser.Char_literContext?): ASTNode {
        return CharLiteralAST(ast, )
    }

    override fun visitStr_liter(ctx: WaccParser.Str_literContext?): ASTNode {
        return super.visitStr_liter(ctx)
    }

    override fun visitPair_liter(ctx: WaccParser.Pair_literContext?): ASTNode {
        return super.visitPair_liter(ctx)
    }

    override fun visitIdent(ctx: WaccParser.IdentContext?): ASTNode {
        return super.visitIdent(ctx)
    }

    override fun visitArray_elem(ctx: WaccParser.Array_elemContext?): ASTNode {
        return super.visitArray_elem(ctx)
    }

    override fun visitIfStat(ctx: WaccParser.IfStatContext): ASTNode {
        val condExpr = visitExpr(ctx.expr()) as ExpressionAST

        if (condExpr.type !is BoolType) {
            throw TypeError(
                "type of conditional expression should be " +
                    "bool and is ${condExpr.type}"
            )
        }

        symbols = symbols.subScope()
        val thenStat = visit(ctx.stat(0))
        if (thenStat !is StatementAST) {
            throw DeclarationError(
                "invalid then statement in if block"
            )
        }
        symbols = symbols.parentScope()!!

        symbols = symbols.subScope()
        val elseStat = visit(ctx.stat(1))
        if (elseStat !is StatementAST) {
            throw DeclarationError(
                "invalid else statement in if block"
            )
        }
        symbols = symbols.parentScope()!!

        return addToScope(IfBlockAST(scope, symbols, condExpr, thenStat, elseStat))
    }

    override fun visitWhileStat(ctx: WaccParser.WhileStatContext): ASTNode {
        val condExpr = visitExpr(ctx.expr()) as ExpressionAST

        if (condExpr.type !is BoolType) {
            throw TypeError(
                "type of conditional expression should be " +
                    "bool and is ${condExpr.type}"
            )
        }

        val whileBlock = WhileBlockAST(scope, symbols, condExpr)

        scope = whileBlock
        symbols = symbols.subScope()
        visit(ctx.stat()) as StatementAST
        symbols = symbols.parentScope()!!
        scope = scope.parent!! as BlockAST

        return addToScope(whileBlock)
    }

    override fun visitPrintStat(ctx: WaccParser.PrintStatContext): ASTNode {
        val expr = visitExpr(ctx.expr()) as ExpressionAST
        when (expr.type) {
            is PairType -> {
                throw TypeError("cannot print pair type: ${expr.type}")
            }
            is ArrayType -> {
                throw TypeError("cannot print array type: ${expr.type}")
            }
        }

        return addToScope(PrintStatementAST(scope, symbols, expr))
    }

    override fun visitExitStat(ctx: WaccParser.ExitStatContext): ASTNode {
        val expr = visitExpr(ctx.expr()) as ExpressionAST
        if (expr.type !is IntType) {
            throw TypeError(
                "expression passed to exit must be an int; type passed is ${expr.type}"
            )
        }
        return addToScope(ExitStatementAST(scope, expr))
    }

    override fun visitReturn_stat(ctx: WaccParser.Return_statContext): ASTNode {
        log("Visiting return statement")

        var enclosingAST: ASTNode? = scope

        while (enclosingAST != null && enclosingAST !is FunctionDeclarationAST) {
            enclosingAST = enclosingAST.parent
        }

        if (enclosingAST == null) {
            throw IllegalStatementError(
                "return statement only allowed inside function definition"
            )
        }

        val func = enclosingAST as FunctionDeclarationAST
        val returnType = symbolTable.lookupAll(func.returnTypeName)

        log(" || return statement is under function ${func.funcName}")

        val expr = visit(ctx.expr()) as ExpressionAST

        when {
            returnType != expr.type -> {
                throw TypeError(
                    "return expression type does not match function return type"
                )
            }
        }

        val returnStat = ReturnStatementAST(func, symbols, expr)
        func.returnStat = returnStat

        return addToScope(returnStat)
    }

    private fun addToScope(stat: StatementAST): StatementAST {
        scope.statements.add(stat)
        return stat
    }
}
