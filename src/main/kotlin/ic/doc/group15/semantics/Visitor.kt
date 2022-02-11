package ic.doc.group15.semantics

import ic.doc.group15.antlr.WaccParser
import ic.doc.group15.antlr.WaccParser.FuncContext
import ic.doc.group15.antlr.WaccParserBaseVisitor
import ic.doc.group15.semantics.ast.* // ktlint-disable no-unused-imports
import java.util.*
import java.util.logging.ConsoleHandler
import java.util.logging.Handler
import java.util.logging.Level
import java.util.logging.Logger

class Visitor(
    private val topAst: AST,
    private val topSymbolTable: SymbolTable
) : WaccParserBaseVisitor<ASTNode>() {

    private var scopeAST: BlockAST = topAst
    private var symbolTable = topSymbolTable

    private val functionsToVisit: MutableMap<String, FuncContext> = mutableMapOf()

    companion object {
        private val LOG = Logger.getLogger(Visitor::class.java.name)
        private val handler: Handler = ConsoleHandler()

        init {
            handler.level = Level.ALL
            LOG.addHandler(handler)
            LOG.level = Level.ALL
        }

        private fun log(message: String) {
//            LOG.log(Level.FINE, message.trimMargin())
            println(message.trimMargin())
        }
    }

    override fun visitBeginEndStat(ctx: WaccParser.BeginEndStatContext): ASTNode {
        symbolTable = symbolTable.subScope()
        val node = visit(ctx.stat())
        symbolTable = symbolTable.parentScope()!!

        return node
    }

    //region statements_and_blocks

    override fun visitProgram(ctx: WaccParser.ProgramContext): ASTNode {
        assert(symbolTable.isTopLevel())

        log(
            """----------------
               |Begin program semantic analysis
            """
        )
        val program = scopeAST as AST

        log("Getting function names")
        for (func in ctx.func()) {
            functionsToVisit[func.ident().text] = func
        }
        log("Function names noted")

        log("Visiting function definitions")
        for (func in ctx.func()) {
            program.statements.add(visitFunc(func) as FunctionDeclarationAST)
        }

        log("Visiting main program")
        visit(ctx.stat())

        log(
            """Semantic analysis complete!
               |----------------
               |
            """
        )

        return program
    }

    // TODO: handle array and pair return types
    override fun visitFunc(ctx: FuncContext): ASTNode {
        val funcName = ctx.ident().text
        log(
            """Visiting function declaration
                || Function name: $funcName
            """
        )

        val returnTypeName = ctx.type().text

        log(""" || Return type: $returnTypeName""")

        if (!symbolTable.isTopLevel()) {
            throw DeclarationError(
                "line: ${ctx.getStart().line} column: ${ctx.getStart().charPositionInLine} " +
                    "functions cannot be declared in this scope"
            )
        }

        val t = TypeParser.parse(symbolTable, ctx.type())
        val f = symbolTable.lookup(funcName)

        when {
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

        val func = FunctionDeclarationAST(scopeAST, symbolTable, t, funcName)

        log(
            """Visiting parameters of function ${func.funcName}"""
        )

        // Add function identifier to symbol table so that it can be recursively accessed

        val parentScope = symbolTable

        symbolTable = symbolTable.subScope()
        scopeAST = func
        for (param in ctx.param()) {
            func.formals.add(visitParam(param) as ParameterAST)
        }
        func.funcIdent = FunctionType(
            t as ReturnableType,
            func.formals.map { p -> p.paramIdent },
            symbolTable
        )
        parentScope.add(funcName, func.funcIdent)
        log("|| Visiting $funcName function body")
        if (ctx.stat() != null) {
            visit(ctx.stat())
        }
        symbolTable = symbolTable.parentScope()!!
        scopeAST = scopeAST.parent!!

        return func
    }

    override fun visitParam(ctx: WaccParser.ParamContext): ASTNode {
        val type = ctx.type()
        val typeName = type.text
        val paramName = ctx.ident().text

        log(
            """Visiting parameter
                || Type name: $typeName
                || Param name: $paramName
            """
        )

        val parameterAST =
            ParameterAST(scopeAST, symbolTable, typeName, paramName)

        val t = TypeParser.parse(symbolTable, type)
        val p = symbolTable.lookup(paramName)

        when {
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
        log("Visiting if statement")

        log("Visiting if statement condition expression")
        val condExpr = visit(ctx.expr()) as ExpressionAST

        if (condExpr.type != BasicType.BoolType) {
            throw TypeError(
                "line: ${ctx.expr().getStart().line} column: ${
                ctx.expr().getStart().charPositionInLine
                } type of conditional expression should be " +
                    "bool and is ${condExpr.type}"
            )
        }

        symbolTable = symbolTable.subScope()
        log("|| Visiting then block")
        val thenStat = visit(ctx.stat(0))
        if (thenStat !is StatementAST) {
            throw DeclarationError(
                "line: ${ctx.stat(0).getStart().line} column: ${
                ctx.stat(0).getStart().charPositionInLine
                } invalid then statement in if block"
            )
        }
        symbolTable = symbolTable.parentScope()!!

        symbolTable = symbolTable.subScope()
        log("|| Visiting else block")
        val elseStat = visit(ctx.stat(1))
        if (elseStat !is StatementAST) {
            throw DeclarationError(
                "line: ${ctx.stat(1).getStart().line} column: ${
                ctx.stat(1).getStart().charPositionInLine
                } invalid else statement in if block"
            )
        }
        symbolTable = symbolTable.parentScope()!!

        return addToScope(IfBlockAST(scopeAST, symbolTable, condExpr, thenStat, elseStat))
    }

    override fun visitWhileStat(ctx: WaccParser.WhileStatContext): ASTNode {
        log("Visiting while statement")

        log("|| Visiting while condition expression")
        val condExpr = visit(ctx.expr()) as ExpressionAST

        if (condExpr.type != BasicType.BoolType) {
            throw TypeError(
                "line: ${ctx.expr().getStart().line} column: ${
                ctx.expr().getStart().charPositionInLine
                } type of conditional expression should be " +
                    "bool and is ${condExpr.type}"
            )
        }

        val whileBlock = WhileBlockAST(scopeAST, symbolTable, condExpr)

        scopeAST = whileBlock
        symbolTable = symbolTable.subScope()
        log("|| Visiting while block")
        visit(ctx.stat()) as StatementAST
        symbolTable = symbolTable.parentScope()!!
        scopeAST = scopeAST.parent!!

        return addToScope(whileBlock)
    }

    override fun visitPrintStat(ctx: WaccParser.PrintStatContext): ASTNode {
        log("Visiting print statement")

        val expr = visit(ctx.expr()) as ExpressionAST

        return addToScope(PrintStatementAST(scopeAST, symbolTable, expr))
    }

    override fun visitPrintlnStat(ctx: WaccParser.PrintlnStatContext): ASTNode {
        log("Visiting println statement")

        val expr = visit(ctx.expr()) as ExpressionAST

        return addToScope(PrintlnStatementAST(scopeAST, symbolTable, expr))
    }

    override fun visitExitStat(ctx: WaccParser.ExitStatContext): ASTNode {
        log("Visiting exit statement")

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
                "line: ${ctx.getStart().line} column: ${ctx.getStart().charPositionInLine} return " +
                    "statement only allowed inside function definition"
            )
        }

        val func = enclosingAST as FunctionDeclarationAST
        val returnType = func.returnType

        log(" || return statement is under function ${func.funcName}")

        val expr = visit(ctx.expr()) as ExpressionAST

        when {
            !returnType.compatible(expr.type) -> {
                throw TypeError(
                    "line: ${ctx.expr().getStart().line} column: ${
                    ctx.expr().getStart().charPositionInLine
                    } return expression type ${expr.type} does not match function return " +
                    "type $returnType"
                )
            }
        }

        val returnStat = ReturnStatementAST(func, symbolTable, expr)
        func.returnStat = returnStat

        return addToScope(returnStat)
    }

    override fun visitFreeStat(ctx: WaccParser.FreeStatContext): ASTNode {
        log("Visiting free statement")

        val expr = visit(ctx.expr()) as ExpressionAST
        if (expr.type !is HeapAllocatedType) {
            throw TypeError(
                "free can only be called on pairs or arrays"
            )
        }

        return FreeStatementAST(scopeAST, symbolTable, expr)
    }

    override fun visitSkipStat(ctx: WaccParser.SkipStatContext?): ASTNode {
        log("Visiting skip statement")

        return addToScope(SkipStatementAST(scopeAST))
    }

    override fun visitReadStat(ctx: WaccParser.ReadStatContext): ASTNode {
        log("Visiting read statement")

        val target = visit(ctx.assign_lhs()) as AssignmentAST
        if (target.type !is BasicType) {
            throw TypeError("cannot read input into target of type ${target.type}")
        }

        log("|| Target type: ${target.type}")

        return ReadStatementAST(scopeAST, symbolTable, target)
    }

    //endregion

    //region assign_and_declare

    override fun visitDeclarationStat(ctx: WaccParser.DeclarationStatContext): ASTNode {
        val type = ctx.type()
        val typeName = type.text
        val ident = ctx.ident()
        val varName = ident.text

        log(
            """Visiting variable declaration 
                || Type name: $typeName
                || Var name: $varName
            """
        )

        val t = TypeParser.parse(symbolTable, type)
        val v = symbolTable.lookup(varName)

        when {
            !t.compatible(t) -> {
                throw TypeError(
                    "line: ${type.getStart().line} column: ${
                    type.getStart().charPositionInLine
                    } return expression type does not match function return type"
                )
            }
        }

        val varDecl = VariableDeclarationAST(scopeAST, symbolTable, typeName, varName)
        varDecl.varIdent = Variable(t)
        symbolTable.add(varName, varDecl.varIdent)

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
        val ident = visitIdent(ctx.ident()) as VariableIdentifierAST

        return AssignToIdentAST(scopeAST, ident)
    }

    override fun visitArrayElemAssignLhs(ctx: WaccParser.ArrayElemAssignLhsContext): ASTNode {
        return AssignToArrayElemAST(scopeAST, visitArray_elem(ctx.array_elem()) as ArrayElemAST)
    }

    override fun visitPairElemAssignLhs(ctx: WaccParser.PairElemAssignLhsContext): ASTNode {
        return AssignToPairElemAST(scopeAST, visit(ctx.pair_elem()) as PairElemAST)
    }

    override fun visitArray_elem(ctx: WaccParser.Array_elemContext): ASTNode {
        val arr = (symbolTable.lookupAll(ctx.ident().text) as Variable).type
            as ArrayType
        val indexList = mutableListOf<ExpressionAST>()

        for (expr in ctx.expr()) {
            val indexExpr = visit(expr) as ExpressionAST
            if (indexExpr.type != BasicType.IntType) {
                throw TypeError("array index must be an int")
            }
            indexList.add(indexExpr)
        }

        return ArrayElemAST(
            symbolTable, ctx.ident().text, indexList,
            arr.elementType
        )
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

        return FstPairElemAST(symbolTable, expr)
    }

    override fun visitSndPair(ctx: WaccParser.SndPairContext): ASTNode {
        val expr = visit(ctx.expr()) as ExpressionAST
        if (expr.type !is PairType) {
            throw TypeError("snd can only be called on pairs")
        }

        return SndPairElemAST(symbolTable, expr)
    }

    override fun visitCallAssignRhs(ctx: WaccParser.CallAssignRhsContext): ASTNode {
        val funcName = ctx.ident().text
        log("Visiting $funcName function call")
        var f = symbolTable.lookupAll(funcName)

        val args: MutableList<WaccParser.ExprContext> = ctx.arg_list().expr()

        when (f) {
            null -> {
                if (functionsToVisit.containsKey(funcName)) {
                    log("Function $funcName called before it was defined!")
                    val oldAst = scopeAST
                    val oldSt = symbolTable
                    // Go back to the top scope as that is the only place where functions can be
                    // defined
                    scopeAST = topAst
                    symbolTable = topSymbolTable
                    f = (visitFunc(functionsToVisit[funcName]!!) as FunctionDeclarationAST)
                        .funcIdent
                    scopeAST = oldAst
                    symbolTable = oldSt
                    functionsToVisit.remove(funcName)
                }

                throw IdentifierError(
                    "line: ${ctx.ident().getStart().line} column: ${
                    ctx.ident().getStart().charPositionInLine
                    } function $funcName not found"
                )
            }
            !is FunctionType -> {
                throw TypeError(
                    "line: ${ctx.ident().getStart().line} column: ${
                    ctx.ident().getStart().charPositionInLine
                    } $funcName is not a function"
                )
            }
        }

        if ((f as FunctionType).formals.size != args.size) {
            throw ParameterError(
                "line: ${ctx.ident().getStart().line} column: ${
                ctx.ident().getStart().charPositionInLine
                } wrong number of arguments for $funcName: " +
                    "expected ${f.formals.size}, got ${args.size}"
            )
        }

        val funcCall = CallAST(symbolTable, funcName, f as FunctionType)

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

    override fun visitIdent(ctx: WaccParser.IdentContext): ASTNode {
        log("Visiting identifier with name ${ctx.text}")
        val ident = symbolTable.lookupAll(ctx.text)
        if (ident == null) {
            throw IdentifierError(
                "identifier ${ctx.text} not defined"
            )
        } else if (ident !is Variable) {
            throw TypeError(
                "identifier must be a variable"
            )
        }
        log("Identifier is variable with type ${ident.type}")
        return VariableIdentifierAST(symbolTable, ctx.text, ident)
    }

    override fun visitInt_liter(ctx: WaccParser.Int_literContext): ASTNode {
        return visit(ctx.children[0])
    }

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
        return CharLiteralAST(ctx.text.substring(1, ctx.text.length - 1)[0])
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

        return ArrayLiteralAST(symbolTable, elemType, elems)
    }

    override fun visitUnaryExpr(ctx: WaccParser.UnaryExprContext): ASTNode? {
        val unOp: UnaryOp

        when {
            ctx.BANG() != null -> {
                unOp = UnaryOp.BANG
            }
            ctx.MINUS() != null -> {
                unOp = UnaryOp.MINUS
            }
            ctx.LEN() != null -> {
                unOp = UnaryOp.LEN
            }
            ctx.ORD() != null -> {
                unOp = UnaryOp.ORD
            }
            ctx.CHR() != null -> {
                unOp = UnaryOp.CHR
            }
            else -> {
                return null
            }
        }

        return visitUnaryExprHelper(unOp, ctx.expr())
    }

    override fun visitBinaryExpr(ctx: WaccParser.BinaryExprContext): ASTNode? {
        val binOp: BinaryOp

        when {
            ctx.MULT() != null -> {
                binOp = BinaryOp.MULT
            }
            ctx.DIV() != null -> {
                binOp = BinaryOp.DIV
            }
            ctx.MOD() != null -> {
                binOp = BinaryOp.MOD
            }
            ctx.PLUS() != null -> {
                binOp = BinaryOp.PLUS
            }
            ctx.MINUS() != null -> {
                binOp = BinaryOp.MINUS
            }
            ctx.GT() != null -> {
                binOp = BinaryOp.GT
            }
            ctx.GTE() != null -> {
                binOp = BinaryOp.GTE
            }
            ctx.LT() != null -> {
                binOp = BinaryOp.LT
            }
            ctx.LTE() != null -> {
                binOp = BinaryOp.LTE
            }
            ctx.EQUALS() != null -> {
                binOp = BinaryOp.EQUALS
            }
            ctx.NOT_EQUALS() != null -> {
                binOp = BinaryOp.NOT_EQUALS
            }
            ctx.AND() != null -> {
                binOp = BinaryOp.AND
            }
            ctx.OR() != null -> {
                binOp = BinaryOp.OR
            }
            else -> {
                return null
            }
        }

        return visitBinaryExprHelper(binOp, ctx.expr(0), ctx.expr(1))
    }

    override fun visitBracketExpr(ctx: WaccParser.BracketExprContext): ASTNode {
        return visit(ctx.expr())
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

    private fun visitUnaryExprHelper(
        unOp: UnaryOp,
        expr: WaccParser.ExprContext
    ): ASTNode {
        log("Visiting unary operator expression")
        val node = unOp.generateNode(symbolTable, visit(expr) as ExpressionAST)
        log("Found unary operator ${node.operator}")
        return node
    }

    private fun visitBinaryExprHelper(
        binOp: BinaryOp,
        expr1: WaccParser.ExprContext,
        expr2: WaccParser.ExprContext
    ): ASTNode {
        log("Visiting binary operator expression")
        val node = binOp.generateNode(
            symbolTable,
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
