package ic.doc.group15.semantics

import ic.doc.group15.antlr.WaccParser
import ic.doc.group15.antlr.WaccParserBaseVisitor
import java.util.logging.Level
import java.util.logging.Logger

class Visitor(
    private val abstractSyntaxTree: AST,
    private val symbolTable: SymbolTable
) : WaccParserBaseVisitor<ASTNode>() {

    private var scopeAST: BlockAST = abstractSyntaxTree
    private var scopeSymbols = symbolTable

    companion object {
        private val LOG = Logger.getLogger(Visitor::class.java.name)

        private fun log(message: String) {
            LOG.log(Level.FINE, message.trimMargin())
        }
    }

    override fun visitProgram(ctx: WaccParser.ProgramContext): ASTNode {
        assert(scopeSymbols.isTopLevel())

        log("Begin program semantic analysis")
        val program = scopeAST as AST

        log("Visiting function definitions")
        for (func in ctx.func()) {
            program.statements.add(visitFunc(func) as FunctionDeclarationAST)
        }

        log("Visiting main program")
        visit(ctx.stat()) as StatementAST

        return program
    }

    // TODO: handle array and pair return types
    override fun visitFunc(ctx: WaccParser.FuncContext): ASTNode {
        val funcName = ctx.ident().text
        log(
            """Visiting function declaration
                || Function name: $funcName
            """
        )

        val returnTypeName = ctx.type().text

        log(""" || Return type: $returnTypeName""")

        if (!scopeSymbols.isTopLevel()) {
            throw DeclarationError("functions cannot be declared in this scope")
        }

        val t = scopeSymbols.lookupAll(returnTypeName)
        val f = scopeSymbols.lookup(funcName)

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
        }

        val func = FunctionDeclarationAST(
            scopeAST, scopeSymbols, returnTypeName, funcName
        )

        log(
            """Visiting parameters of function ${func.funcName}"""
        )

        scopeSymbols = scopeSymbols.subScope()
        scopeAST = func
        for (param in ctx.param()) {
            func.formals.add(visitParam(param) as ParameterAST)
        }
        scopeSymbols = scopeSymbols.parentScope()!!
        scopeAST = scopeAST.parent!! as BlockAST

        func.funcIdent = FunctionType(
            t as ReturnableType,
            func.formals.map { p -> p.paramIdent },
            scopeSymbols
        )
        symbolTable.add(funcName, func.funcIdent)

        visit(ctx.stat())

        return func
    }

    override fun visitParam(ctx: WaccParser.ParamContext): ASTNode {
        val typeName = ctx.type().text
        val paramName = ctx.ident().text

        log(
            """Visiting parameter
                || Type name: $typeName
                || Param name: $paramName
            """
        )

        val parameterAST = ParameterAST(scopeAST, scopeSymbols, typeName, paramName)

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

    override fun visitDeclarationStat(ctx: WaccParser.DeclarationStatContext): ASTNode {

        val typeName = ctx.type().text
        val varName = ctx.ident().text

        log(
            """Visiting variable declaration 
                || Type name: $typeName
                || Var name: $varName
            """
        )

        val varDecl = VariableDeclarationAST(scopeAST, scopeSymbols, typeName, varName)

        val t = scopeSymbols.lookupAll(typeName)
        val v = scopeSymbols.lookup(varName)

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

        scopeSymbols.add(varName, varDecl.varIdent)

        return varDecl
    }

//    override fun visitAssignmentStat(ctx: WaccParser.AssignmentStatContext): ASTNode {
//
//        val varName = ctx.().text
//        val expr = ctx.expr()
//        val exprName = expr.text
//
//        log(
//            """Visiting variable assignment
//                || Identifier: $varName
//                || Expression: $exprName
//            """.trimIndent()
//        )
//
//        val varAssign = VariableAssignmentAST(scope, symbols, varName, exprName)
//
//        // identifier may be declared in parent scope
//        val v = symbols.lookupAll(varName)
//
//        when {
//            v == null -> {
//                throw DeclarationError("$varName has not been declared")
//            }
//            v !is Variable -> {
//                throw IdentifierError("$varName is not a variable")
//            }
//            v.type != expr.type -> {
//                throw TypeError("$varName type(${v.type}) not compatible with expression type (${expr.type})")
//            }
//            else -> {
//                varAssign.varIdent = expr
//            }
//        }
//
//        st.add(varName, varAssign.varIdent)
//
//        return varAssign
//    }

    override fun visitBangUnaryOp(ctx: WaccParser.BangUnaryOpContext?): ASTNode {
        return UnaryOpAST(scopeAST, symbolTable, UnaryOp.BANG)
    }

    override fun visitMinusUnaryOp(ctx: WaccParser.MinusUnaryOpContext?): ASTNode {
        return UnaryOpAST(scopeAST, symbolTable, UnaryOp.MINUS)
    }

    override fun visitLenUnaryOp(ctx: WaccParser.LenUnaryOpContext?): ASTNode {
        return UnaryOpAST(scopeAST, symbolTable, UnaryOp.LEN)
    }

    override fun visitOrdUnaryOp(ctx: WaccParser.OrdUnaryOpContext?): ASTNode {
        return UnaryOpAST(scopeAST, symbolTable, UnaryOp.ORD)
    }

    override fun visitChrUnaryOp(ctx: WaccParser.ChrUnaryOpContext?): ASTNode {
        return UnaryOpAST(scopeAST, symbolTable, UnaryOp.CHR)
    }

    override fun visitUnaryOpExpr(ctx: WaccParser.UnaryOpExprContext): ASTNode {
        val expr = visit(ctx.expr()) as ExpressionAST
        val unaryOp = visit(ctx.unary_op()) as UnaryOpAST

        when (unaryOp.operator) {
            UnaryOp.BANG -> {
                val expectedType = BasicType.BoolType
                if (expr.type != expectedType) {
                    throw TypeError("type mismatch when evaluating ${unaryOp
                        .operator} - " +
                            "expected: " +
                            "${expectedType}, actual: ${expr.type}")
                }
            }
            UnaryOp.MINUS -> {
                val expectedType = BasicType.IntType
                if (expr.type != expectedType) {
                    throw TypeError("type mismatch when evaluating ${unaryOp
                        .operator} - " +
                            "expected: " +
                            "${expectedType}, actual: ${expr.type}")
                }
            }
            UnaryOp.LEN -> {
                if (expr.type !is ArrayType) {
                    throw TypeError("type mismatch when evaluating ${unaryOp
                        .operator} - " +
                            "expected: " +
                            "ArrayType, actual: ${expr.type}")
                }
            }
            UnaryOp.ORD -> {
                val expectedType = BasicType.CharType
                if (expr.type != expectedType) {
                    throw TypeError("type mismatch when evaluating ${unaryOp
                        .operator} - " +
                            "expected: " +
                            "${expectedType}, actual: ${expr.type}")
                }
            }
            UnaryOp.CHR -> {
                val expectedType = BasicType.IntType
                if (expr.type != expectedType) {
                    throw TypeError("type mismatch when evaluating ${unaryOp
                        .operator} - " +
                            "expected: " +
                            "${expectedType}, actual: ${expr.type}")
                }
            }
        }

        return UnaryOpExprAST(scopeAST, symbolTable, expr, unaryOp)
    }

    override fun visitMultBinaryOp(ctx: WaccParser.MultBinaryOpContext?): ASTNode {
        return BinaryOpAST(scopeAST, symbolTable, BinaryOp.MULT)
    }

    override fun visitDivBinaryOp(ctx: WaccParser.DivBinaryOpContext?): ASTNode {
        return BinaryOpAST(scopeAST, symbolTable, BinaryOp.DIV)
    }

    override fun visitModBinaryOp(ctx: WaccParser.ModBinaryOpContext?): ASTNode {
        return BinaryOpAST(scopeAST, symbolTable, BinaryOp.MOD)
    }

    override fun visitPlusBinaryOp(ctx: WaccParser.PlusBinaryOpContext?): ASTNode {
        return BinaryOpAST(scopeAST, symbolTable, BinaryOp.PLUS)
    }

    override fun visitMinusBinaryOp(ctx: WaccParser.MinusBinaryOpContext?): ASTNode {
        return BinaryOpAST(scopeAST, symbolTable, BinaryOp.MINUS)
    }

    override fun visitGtBinaryOp(ctx: WaccParser.GtBinaryOpContext?): ASTNode {
        return BinaryOpAST(scopeAST, symbolTable, BinaryOp.GT)
    }

    override fun visitGteBinaryOp(ctx: WaccParser.GteBinaryOpContext?): ASTNode {
        return BinaryOpAST(scopeAST, symbolTable, BinaryOp.GTE)
    }

    override fun visitLtBinaryOp(ctx: WaccParser.LtBinaryOpContext?): ASTNode {
        return BinaryOpAST(scopeAST, symbolTable, BinaryOp.LT)
    }

    override fun visitLteBinaryOp(ctx: WaccParser.LteBinaryOpContext?): ASTNode {
        return BinaryOpAST(scopeAST, symbolTable, BinaryOp.LTE)
    }

    override fun visitEqualsBinaryOp(ctx: WaccParser.EqualsBinaryOpContext?): ASTNode {
        return BinaryOpAST(scopeAST, symbolTable, BinaryOp.EQUALS)
    }

    override fun visitNotEqualsBinaryOp(ctx: WaccParser.NotEqualsBinaryOpContext?): ASTNode {
        return BinaryOpAST(scopeAST, symbolTable, BinaryOp.NOT_EQUALS)
    }

    override fun visitAndBinaryOp(ctx: WaccParser.AndBinaryOpContext?): ASTNode {
        return BinaryOpAST(scopeAST, symbolTable, BinaryOp.AND)
    }

    override fun visitOrBinaryOp(ctx: WaccParser.OrBinaryOpContext?): ASTNode {
        return BinaryOpAST(scopeAST, symbolTable, BinaryOp.OR)
    }

    override fun visitBinaryOpExpr(ctx: WaccParser.BinaryOpExprContext?): ASTNode {
        val expr1 = visit(ctx?.expr(0)) as ExpressionAST
        val expr2 = visit(ctx?.expr(1)) as ExpressionAST
        val binaryOp = visit(ctx?.binary_op()) as BinaryOpAST

        if (expr1.type != expr2.type) {
            throw TypeError("operands of binary expressions have different " +
                    "types")
        }

        when (expr1.type) {
            BasicType.IntType -> {
                val disallowedTypes = setOf(BinaryOp.AND, BinaryOp
                    .OR)
                if (disallowedTypes.contains(expr1.type as BinaryOp)) {
                    throw TypeError("cannot apply ${binaryOp.operator} to " +
                            "arguments of type ${expr1.type}")
                }
            }
            BasicType.CharType -> {
                val allowedTypes = setOf(BinaryOp.GT, BinaryOp.GTE, BinaryOp
                    .LT, BinaryOp.LTE, BinaryOp.EQUALS, BinaryOp.NOT_EQUALS)
                if (!allowedTypes.contains(expr1.type as BinaryOp)) {
                    throw TypeError("cannot apply ${binaryOp.operator} to " +
                            "arguments of type ${expr1.type}")
                }
            }
            BasicType.StringType -> {
                val allowedTypes = setOf(BinaryOp.EQUALS, BinaryOp.NOT_EQUALS)
                if (!allowedTypes.contains(expr1.type as BinaryOp)) {
                    throw TypeError("cannot apply ${binaryOp.operator} to " +
                            "arguments of type ${expr1.type}")
                }
            }
            BasicType.BoolType -> {
                val allowedTypes = setOf(BinaryOp.EQUALS, BinaryOp
                    .NOT_EQUALS, BinaryOp.AND, BinaryOp.OR)
                if (!allowedTypes.contains(expr1.type as BinaryOp)) {
                    throw TypeError("cannot apply ${binaryOp.operator} to " +
                            "arguments of type ${expr1.type}")
                }
            }
            is ArrayType -> {
                val allowedTypes = setOf(BinaryOp.EQUALS, BinaryOp.NOT_EQUALS)
                if (!allowedTypes.contains(expr1.type as BinaryOp)) {
                    throw TypeError("cannot apply ${binaryOp.operator} to " +
                            "arguments of type ${expr1.type}")
                }
            }
            is PairType -> {
                val allowedTypes = setOf(BinaryOp.EQUALS, BinaryOp.NOT_EQUALS)
                if (!allowedTypes.contains(expr1.type as BinaryOp)) {
                    throw TypeError("cannot apply ${binaryOp.operator} to " +
                            "arguments of type ${expr1.type}")
                }
            }
        }

        return BinaryOpExprAST(scopeAST, symbolTable, expr1, expr2, binaryOp)
    }

    override fun visitArray_elem(ctx: WaccParser.Array_elemContext?): ASTNode {
        return super.visitArray_elem(ctx)
    }



    override fun visitIfStat(ctx: WaccParser.IfStatContext): ASTNode {
        val condExpr = visit(ctx.expr()) as ExpressionAST

        if (condExpr.type != BasicType.BoolType) {
            throw TypeError(
                "type of conditional expression should be " +
                    "bool and is ${condExpr.type}"
            )
        }

        scopeSymbols = scopeSymbols.subScope()
        val thenStat = visit(ctx.stat(0))
        if (thenStat !is StatementAST) {
            throw DeclarationError(
                "invalid then statement in if block"
            )
        }
        scopeSymbols = scopeSymbols.parentScope()!!

        scopeSymbols = scopeSymbols.subScope()
        val elseStat = visit(ctx.stat(1))
        if (elseStat !is StatementAST) {
            throw DeclarationError(
                "invalid else statement in if block"
            )
        }
        scopeSymbols = scopeSymbols.parentScope()!!

        return addToScope(IfBlockAST(scopeAST, scopeSymbols, condExpr, thenStat, elseStat))
    }

    override fun visitWhileStat(ctx: WaccParser.WhileStatContext): ASTNode {
        val condExpr = visit(ctx.expr()) as ExpressionAST

        if (condExpr.type != BasicType.BoolType) {
            throw TypeError(
                "type of conditional expression should be " +
                    "bool and is ${condExpr.type}"
            )
        }

        val whileBlock = WhileBlockAST(scopeAST, scopeSymbols, condExpr)

        scopeAST = whileBlock
        scopeSymbols = scopeSymbols.subScope()
        visit(ctx.stat()) as StatementAST
        scopeSymbols = scopeSymbols.parentScope()!!
        scopeAST = scopeAST.parent!! as BlockAST

        return addToScope(whileBlock)
    }

    override fun visitPrintStat(ctx: WaccParser.PrintStatContext): ASTNode {
        val expr = visit(ctx.expr()) as ExpressionAST
        when (expr.type) {
            is PairType -> {
                throw TypeError("cannot print pair type: ${expr.type}")
            }
            is ArrayType -> {
                throw TypeError("cannot print array type: ${expr.type}")
            }
        }

        return addToScope(PrintStatementAST(scopeAST, scopeSymbols, expr))
    }

    override fun visitExitStat(ctx: WaccParser.ExitStatContext): ASTNode {
        val expr = visit(ctx.expr()) as ExpressionAST
        if (expr.type != BasicType.IntType) {
            throw TypeError(
                "expression passed to exit must be an int; type passed is ${expr.type}"
            )
        }
        return addToScope(ExitStatementAST(scopeAST, expr))
    }

    override fun visitReturn_stat(ctx: WaccParser.Return_statContext): ASTNode {
        log("Visiting return statement")

        var enclosingAST: ASTNode? = scopeAST

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

        val returnStat = ReturnStatementAST(func, scopeSymbols, expr)
        func.returnStat = returnStat

        return addToScope(returnStat)
    }

//    override fun visitFreeStat(ctx: WaccParser.FreeStatContext): ASTNode {
//        val expr = visitExpr(ctx.expr()) as ExpressionAST
//
//        when {
//            v == null -> {
//                throw IdentifierError(
//                    "trying to free $varName, which has not" +
//                            " been declared"
//                )
//            }
//            !(v is PairType || v is ArrayType) -> {
//                throw TypeError(
//                    "trying to free $varName, which is neither a " +
//                            "pair nor an array"
//                )
//            }
//        }
//
//        return addToScope(FreeStatementAST(scope, symbols, expr))
//    }

    override fun visitSkipStat(ctx: WaccParser.SkipStatContext?): ASTNode {
        return addToScope(SkipStatementAST(scopeAST))
    }

//    override fun visitReadStat(ctx: WaccParser.ReadStatContext?): ASTNode {
//        val v = symbolTable.lookupAll(varName)
//
//        when {
//            v == null -> {
//                throw IdentifierError("identifier $varName not found")
//            }
//            v !is Variable -> {
//                throw TypeError("$varName is not a variable")
//            }
//            !(
//                    v is IntType || v is CharType || v is StringType || v is PairType ||
//                            v is ArrayType
//                    ) -> {
//                throw TypeError(
//                    "$varName is not an int, char, string, pair " +
//                            "element or array element"
//                )
//            }
//            else -> {
//                varIdent = v
//            }
//        }
//
//        symbolTable.add(varName, varIdent)
//    }

    override fun visitCallAssign(ctx: WaccParser.CallAssignContext): ASTNode {
        val funcName = ctx.ident().text
        val f = symbolTable.lookupAll(funcName)

        val args: MutableList<WaccParser.ExprContext> = ctx.arg_list().expr()

        when {
            f == null -> {
                throw IdentifierError("function $funcName not found")
            }
            f !is FunctionType -> {
                throw TypeError("$funcName is not a function")
            }
            f.formals.size != args.size -> {
                throw ParameterError(
                    "wrong number of arguments for $funcName: " +
                            "expected ${f.formals.size}, got ${args.size}"
                )
            }
        }

        val funcCall = CallAST(scopeAST, scopeSymbols, funcName)

        funcCall.funcIdent = f as FunctionType

        for (k in args.indices) {
            val argExpr = visit(args[k]) as ExpressionAST
            if (f.formals[k].type::class != argExpr.type::class) {
                throw TypeError(
                    "type of function parameter $k incompatible with " +
                            "declaration of $funcName"
                )
            }
            funcCall.actuals.add(argExpr)
        }

        return funcCall
    }

    private fun addToScope(stat: StatementAST): StatementAST {
        scopeAST.statements.add(stat)
        return stat
    }
}
