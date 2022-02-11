package ic.doc.group15.semantics

import ic.doc.group15.antlr.WaccParser
import ic.doc.group15.antlr.WaccParser.ArrayTypeContext
import ic.doc.group15.antlr.WaccParserBaseVisitor
import ic.doc.group15.semantics.ast.* // ktlint-disable no-unused-imports
import java.util.*
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
            LOG.level = Level.FINE
            LOG.log(Level.FINE, message.trimMargin())
        }
    }

    //region statements_and_blocks

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
            throw DeclarationError("line: ${ctx.getStart().line} column: ${ctx.getStart().charPositionInLine} functions cannot be declared in this scope")
        }

        val t = scopeSymbols.lookupAll(returnTypeName)
        val f = scopeSymbols.lookup(funcName)

        when {
            t == null -> {
                throw TypeError(
                    "line: ${ctx.type().getStart().line} column: ${
                    ctx.type().getStart().charPositionInLine
                    } unknown return type $returnTypeName"
                )
            }
            t !is Type -> {
                throw TypeError(
                    "line: ${ctx.type().getStart().line} column: ${
                    ctx.type().getStart().charPositionInLine
                    } $returnTypeName is not a type"
                )
            }
            t !is ReturnableType -> {
                throw TypeError(
                    "line: ${ctx.type().getStart().line} column: ${
                    ctx.type().getStart().charPositionInLine
                    } cannot return $returnTypeName type"
                )
            }
            f != null -> {
                throw DeclarationError(
                    "line: ${ctx.type().getStart().line} column: ${
                    ctx.type().getStart().charPositionInLine
                    } type().function $funcName already declared"
                )
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
        scopeAST = scopeAST.parent!!

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

        val parameterAST =
            ParameterAST(scopeAST, scopeSymbols, typeName, paramName)

        val t = symbolTable.lookupAll(typeName)
        val p = symbolTable.lookup(paramName)

        when {
            t == null -> {
                throw TypeError(
                    "line: ${ctx.type().getStart().line} column: ${
                    ctx.type().getStart().charPositionInLine
                    } unknown return type $typeName"
                )
            }
            t !is Type -> {
                throw TypeError(
                    "line: ${ctx.type().getStart().line} column: ${
                    ctx.type().getStart().charPositionInLine
                    } $typeName is not a type"
                )
            }
            t !is ReturnableType -> {
                throw TypeError(
                    "line: ${ctx.type().getStart().line} column: ${
                    ctx.type().getStart().charPositionInLine
                    } $typeName cannot be a parameter"
                )
            }
            p != null -> {
                if (p is Param) {
                    throw DeclarationError(
                        "line: ${ctx.ident().getStart().line} column: ${
                        ctx.ident().getStart().charPositionInLine
                        } parameter $paramName already declared for this function"
                    )
                } else {
                    throw DeclarationError(
                        "line: ${ctx.ident().getStart().line} column: ${
                        ctx.ident().getStart().charPositionInLine
                        } identifier $paramName already declared"
                    )
                }
            }
            else -> {
                parameterAST.paramIdent = Param(t)
            }
        }

        symbolTable.add(paramName, parameterAST.paramIdent)

        return parameterAST
    }

    override fun visitIfStat(ctx: WaccParser.IfStatContext): ASTNode {
        val condExpr = visit(ctx.expr()) as ExpressionAST

        if (condExpr.type != BasicType.BoolType) {
            throw TypeError(
                "line: ${ctx.expr().getStart().line} column: ${
                ctx.expr().getStart().charPositionInLine
                } type of conditional expression should be " +
                    "bool and is ${condExpr.type}"
            )
        }

        scopeSymbols = scopeSymbols.subScope()
        val thenStat = visit(ctx.stat(0))
        if (thenStat !is StatementAST) {
            throw DeclarationError(
                "line: ${ctx.stat(0).getStart().line} column: ${
                ctx.stat(0).getStart().charPositionInLine
                } invalid then statement in if block"
            )
        }
        scopeSymbols = scopeSymbols.parentScope()!!

        scopeSymbols = scopeSymbols.subScope()
        val elseStat = visit(ctx.stat(1))
        if (elseStat !is StatementAST) {
            throw DeclarationError(
                "line: ${ctx.stat(1).getStart().line} column: ${
                ctx.stat(1).getStart().charPositionInLine
                } invalid else statement in if block"
            )
        }
        scopeSymbols = scopeSymbols.parentScope()!!

        return addToScope(IfBlockAST(scopeAST, scopeSymbols, condExpr, thenStat, elseStat))
    }

    override fun visitWhileStat(ctx: WaccParser.WhileStatContext): ASTNode {
        val condExpr = visit(ctx.expr()) as ExpressionAST

        if (condExpr.type != BasicType.BoolType) {
            throw TypeError(
                "line: ${ctx.expr().getStart().line} column: ${
                ctx.expr().getStart().charPositionInLine
                } type of conditional expression should be " +
                    "bool and is ${condExpr.type}"
            )
        }

        val whileBlock = WhileBlockAST(scopeAST, scopeSymbols, condExpr)

        scopeAST = whileBlock
        scopeSymbols = scopeSymbols.subScope()
        visit(ctx.stat()) as StatementAST
        scopeSymbols = scopeSymbols.parentScope()!!
        scopeAST = scopeAST.parent!!

        return addToScope(whileBlock)
    }

    override fun visitPrintStat(ctx: WaccParser.PrintStatContext): ASTNode {
        val expr = visit(ctx.expr()) as ExpressionAST
        when (expr.type) {
            is PairType -> {
                throw TypeError(
                    "line: ${ctx.expr().getStart().line} column: ${
                    ctx.expr().getStart().charPositionInLine
                    } cannot print pair type: ${expr.type}"
                )
            }
            is ArrayType -> {
                throw TypeError(
                    "line: ${ctx.expr().getStart().line} column: ${
                    ctx.expr().getStart().charPositionInLine
                    } expr().cannot print array type: ${expr.type}"
                )
            }
        }

        return addToScope(PrintStatementAST(scopeAST, scopeSymbols, expr))
    }

    override fun visitPrintlnStat(ctx: WaccParser.PrintlnStatContext): ASTNode {
        val expr = visit(ctx.expr()) as ExpressionAST
        when (expr.type) {
            is PairType -> {
                throw TypeError("cannot print pair type: ${expr.type}")
            }
            is ArrayType -> {
                throw TypeError("cannot print array type: ${expr.type}")
            }
        }

        return addToScope(PrintlnStatementAST(scopeAST, scopeSymbols, expr))
    }

    override fun visitExitStat(ctx: WaccParser.ExitStatContext): ASTNode {
        val expr = visit(ctx.expr()) as ExpressionAST
        if (expr.type != BasicType.IntType) {
            throw TypeError(
                "line: ${ctx.expr().getStart().line} column: ${
                ctx.expr().getStart().charPositionInLine
                } expression passed to exit must be an int; type passed is ${expr.type}"
            )
        }
        return addToScope(ExitStatementAST(scopeAST, expr))
    }

    override fun visitReturn_stat(ctx: WaccParser.Return_statContext): ASTNode {
        log("Visiting return statement")

        var enclosingAST: BlockAST? = scopeAST

        while (enclosingAST != null && enclosingAST !is FunctionDeclarationAST) {
            enclosingAST = enclosingAST.parent
        }

        if (enclosingAST == null) {
            throw IllegalStatementError(
                "line: ${ctx.getStart().line} column: ${ctx.getStart().charPositionInLine} return statement only allowed inside function definition"
            )
        }

        val func = enclosingAST as FunctionDeclarationAST
        val returnType = symbolTable.lookupAll(func.returnTypeName)

        log(" || return statement is under function ${func.funcName}")

        val expr = visit(ctx.expr()) as ExpressionAST

        when {
            returnType != expr.type -> {
                throw TypeError(
                    "line: ${ctx.expr().getStart().line} column: ${
                    ctx.expr().getStart().charPositionInLine
                    } return expression type does not match function return type"
                )
            }
        }

        val returnStat = ReturnStatementAST(func, scopeSymbols, expr)
        func.returnStat = returnStat

        return addToScope(returnStat)
    }

    override fun visitFreeStat(ctx: WaccParser.FreeStatContext): ASTNode {
        val expr = visit(ctx.expr()) as ExpressionAST
        if (expr.type !is HeapAllocatedType) {
            throw TypeError(
                "free can only be called on pairs or arrays"
            )
        }

        return FreeStatementAST(scopeAST, scopeSymbols, expr)
    }

    override fun visitSkipStat(ctx: WaccParser.SkipStatContext?): ASTNode {
        return addToScope(SkipStatementAST(scopeAST))
    }

    override fun visitReadStat(ctx: WaccParser.ReadStatContext): ASTNode {
        val target = visit(ctx.assign_lhs()) as AssignmentAST
        if (target.type !is BasicType) {
            throw TypeError("cannot read input into target of type ${target.type}")
        }
        return ReadStatementAST(scopeAST, symbolTable, target)
    }

    //endregion

    //region assign_and_declare

    override fun visitDeclarationStat(ctx: WaccParser.DeclarationStatContext): ASTNode {
        val type = ctx.type()
        val typeName = type.text
        val ident = ctx.ident()
        val varName = ident.text
        val rhs = visit(ident)

        log(
            """Visiting variable declaration 
                || Type name: $typeName
                || Var name: $varName
            """
        )

        val t = scopeSymbols.lookupAll(typeName)
        val v = scopeSymbols.lookup(varName)

        when {
            t == null -> {
                throw TypeError(
                    "line: ${type.getStart().line} column: ${
                    type.getStart().charPositionInLine
                    } unknown type $typeName"
                )
            }
            t !is Type -> {
                throw TypeError(
                    "line: ${type.getStart().line} column: ${
                    type.getStart().charPositionInLine
                    } $typeName is not a type"
                )
            }
            v != null -> {
                throw DeclarationError(
                    "line: ${ident.getStart().line} column: ${
                    ident.getStart().charPositionInLine
                    } $varName has already been declared"
                )
            }
            !t.compatible(rhs) -> {
                throw TypeError(
                    "line: ${type.getStart().line} column: ${
                        type.getStart().charPositionInLine
                    } return expression type does not match function return type"
                )
            }
        }

        val varDecl = VariableDeclarationAST(scopeAST, scopeSymbols, typeName, varName)
        varDecl.varIdent = Variable(t as Type)
        scopeSymbols.add(varName, varDecl.varIdent)

        return varDecl
    }

    override fun visitAssignmentStat(ctx: WaccParser.AssignmentStatContext): ASTNode {
        val assignLhs = visit(ctx.assign_lhs()) as AssignmentAST
        val assignRhs = visit(ctx.assign_rhs()) as AssignRhsAST

        if (!assignLhs.type.compatible(assignRhs.type)) {
            throw TypeError(
                "trying to assign rhs of type ${assignRhs.type} to lhs of type " +
                    "${assignLhs.type} is not allowed"
            )
        }

        assignLhs.rhs = assignRhs

        return assignLhs
    }

    override fun visitIdentAssignLhs(ctx: WaccParser.IdentAssignLhsContext): ASTNode {
        val ident = visitIdent(ctx.ident())
        if (ident !is VariableIdentifierAST) {
            throw TypeError(
                "only variables can be assigned to"
            )
        }
        return AssignToIdentAST(scopeAST, ident)
    }

    override fun visitArrayElemAssignLhs(ctx: WaccParser.ArrayElemAssignLhsContext): ASTNode {
        return AssignToArrayElemAST(scopeAST, visitArray_elem(ctx.array_elem()) as ArrayElemAST)
    }

    override fun visitPairElemAssignLhs(ctx: WaccParser.PairElemAssignLhsContext): ASTNode {
        return AssignToPairElemAST(scopeAST, visit(ctx.pair_elem()) as PairElemAST)
    }

    override fun visitArray_elem(ctx: WaccParser.Array_elemContext): ASTNode {
        var arrayExpr = visit(ctx.ident()) as ExpressionAST

        for (expr in ctx.expr()) {
            if (arrayExpr.type !is ArrayType) {
                throw TypeError("cannot use array index on non-array type")
            }
            val indexExpr = visit(expr) as ExpressionAST
            if (indexExpr.type != BasicType.IntType) {
                throw TypeError("array index must be an int")
            }
            arrayExpr = ArrayElemAST(scopeSymbols, arrayExpr, indexExpr)
        }
        assert(arrayExpr is ArrayElemAST)

        return arrayExpr
    }

    override fun visitNewPairAssignRhs(ctx: WaccParser.NewPairAssignRhsContext): ASTNode {
        val expr1 = visit(ctx.expr(0)) as ExpressionAST
        val expr2 = visit(ctx.expr(1)) as ExpressionAST
        return NewPairAST(symbolTable, expr1, expr2)
    }

    override fun visitFstPair(ctx: WaccParser.FstPairContext): ASTNode {
        val expr = visit(ctx.expr()) as ExpressionAST
        if (expr.type !is PairType) {
            throw TypeError("fst can only be called on pairs")
        }

        return FstPairElemAST(scopeSymbols, expr)
    }

    override fun visitSndPair(ctx: WaccParser.SndPairContext): ASTNode {
        val expr = visit(ctx.expr()) as ExpressionAST
        if (expr.type !is PairType) {
            throw TypeError("snd can only be called on pairs")
        }

        return SndPairElemAST(scopeSymbols, expr)
    }

    override fun visitCallAssignRhs(ctx: WaccParser.CallAssignRhsContext): ASTNode {
        val funcName = ctx.ident().text
        val f = symbolTable.lookupAll(funcName)

        val args: MutableList<WaccParser.ExprContext> = ctx.arg_list().expr()

        when {
            f == null -> {
                throw IdentifierError(
                    "line: ${ctx.ident().getStart().line} column: ${
                    ctx.ident().getStart().charPositionInLine
                    } function $funcName not found"
                )
            }
            f !is FunctionType -> {
                throw TypeError(
                    "line: ${ctx.ident().getStart().line} column: ${
                    ctx.ident().getStart().charPositionInLine
                    } $funcName is not a function"
                )
            }
            f.formals.size != args.size -> {
                throw ParameterError(
                    "line: ${ctx.ident().getStart().line} column: ${
                    ctx.ident().getStart().charPositionInLine
                    } wrong number of arguments for $funcName: " +
                        "expected ${f.formals.size}, got ${args.size}"
                )
            }
        }

        val funcCall = CallAST(scopeSymbols, funcName, f as FunctionType)

        for (k in args.indices) {
            val argExpr = visit(args[k]) as ExpressionAST
            if (f.formals[k].type::class != argExpr.type::class) {
                throw TypeError(
                    "line: ${ctx.ident().getStart().line} column: ${
                    ctx.ident().getStart().charPositionInLine
                    } type of function parameter $k incompatible with " +
                        "declaration of $funcName"
                )
            }
            funcCall.actuals.add(argExpr)
        }

        return funcCall
    }

    //endregion

    //region literals

    override fun visitInt_liter_positive(ctx: WaccParser.Int_liter_positiveContext): ASTNode {
        val i = parseInt(ctx.POSITIVE_INTEGER().text)
        assert(i in 0..INT_MAX)

        return IntLiteralAST(i)
    }

    override fun visitInt_liter_negative(ctx: WaccParser.Int_liter_negativeContext): ASTNode {
        val i = parseInt(ctx.text)
        assert(i in INT_MIN..0)

        return IntLiteralAST(i)
    }

    override fun visitTBool(ctx: WaccParser.TBoolContext?): ASTNode {
        return BoolLiteralAST(true)
    }

    override fun visitFBool(ctx: WaccParser.FBoolContext?): ASTNode {
        return BoolLiteralAST(false)
    }

    override fun visitChar_liter(ctx: WaccParser.Char_literContext): ASTNode {
        return CharLiteralAST(ctx.text.single())
    }

    override fun visitStr_liter(ctx: WaccParser.Str_literContext): ASTNode {
        return StringLiteralAST(ctx.text)
    }

    override fun visitPair_liter(ctx: WaccParser.Pair_literContext): ASTNode {
        return NullPairLiteralAST()
    }

    override fun visitArray_liter(ctx: WaccParser.Array_literContext): ASTNode {
        assert(ctx.expr().isNotEmpty())

        val elems: MutableList<ExpressionAST> = LinkedList()

        var elemType = Type.ANY
        for (expr in ctx.expr()) {
            val elem = visit(expr) as ExpressionAST
            if (!elemType.compatible(elem.type)) {
                throw TypeError(
                    "line: ${ctx.getStart().line} column: ${
                    ctx.getStart().charPositionInLine
                    } elements in array literal must all by the same type"
                )
            }
            elemType = elem.type
            elems.add(elem)
        }

        return ArrayLiteralAST(scopeSymbols, elemType, elems)
    }

    //endregion

    //region helpers

    private fun parseInt(text: String): Int {
        try {
            return Integer.parseInt(text)
        } catch (e: NumberFormatException) {
            throw OutOfBoundsError(
                "integer value $text is out of bounds; must be 32-bit signed integer"
            )
        }
    }

    private fun visitUnaryExpr(
        unOp: UnaryOp,
        expr: WaccParser.ExprContext
    ): ASTNode {
        log("Visiting unary operator expression")
        val node = unOp.generateNode(scopeSymbols, visit(expr) as ExpressionAST)
        log("Found unary operator ${node.operator}")
        return node
    }

    private fun visitBinaryExpr(
        binOp: BinaryOp,
        expr1: WaccParser.ExprContext,
        expr2: WaccParser.ExprContext
    ): ASTNode {
        log("Visiting binary operator expression")
        val node = binOp.generateNode(
            scopeSymbols,
            visit(expr1) as ExpressionAST,
            visit(expr2) as ExpressionAST
        )
        log("Found binary operator ${node.operator}")
        return node
    }

    private fun addToScope(stat: StatementAST): StatementAST {
        scopeAST.statements.add(stat)
        return stat
    }

    //endregion
}
