package ic.doc.group15.visitor

import ic.doc.group15.SymbolTable
import ic.doc.group15.antlr.WaccParser
import ic.doc.group15.antlr.WaccParser.*
import ic.doc.group15.antlr.WaccParserBaseVisitor
import ic.doc.group15.ast.* // ktlint-disable no-unused-imports
import ic.doc.group15.ast.BinaryOp
import ic.doc.group15.error.SemanticErrorList
import ic.doc.group15.error.semantic.*
import ic.doc.group15.type.* // ktlint-disable no-unused-imports
import ic.doc.group15.type.BasicType.IntType
import java.util.*

class Visitor(
    private val topAst: AST,
    private val topSymbolTable: SymbolTable,
    private val errors: SemanticErrorList,
    private val enableLogging: Boolean = false
) : WaccParserBaseVisitor<ASTNode>() {

    private var scopeAST: BlockAST = topAst
    private var symbolTable = topSymbolTable

    private val functionsToVisit: MutableMap<String, FuncContext> = mutableMapOf()
    private val declaredFunctions: MutableMap<String, FunctionDeclarationAST> = mutableMapOf()

    override fun visitBeginEndStat(ctx: BeginEndStatContext): ASTNode {
        symbolTable = symbolTable.subScope()
        val node = visit(ctx.stat())
        symbolTable = symbolTable.parentScope()!!

        return node
    }

    //region statements_and_blocks

    override fun visitProgram(ctx: ProgramContext): ASTNode {
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

    override fun visitFunc(ctx: FuncContext): ASTNode {
        val ident = ctx.ident()
        val funcName = ident.text

        if (!symbolTable.isTopLevel()) {
            errors.addError(FunctionDeclarationInWrongScopeError(ident.start))
        }

        if (!functionsToVisit.containsKey(funcName)) {
            assert(declaredFunctions.containsKey(funcName))
            log("Function $funcName already declared!")
            return declaredFunctions[funcName]!!
        }

        log(
            """Visiting function declaration
                || Function name: $funcName
            """
        )

        val returnTypeName = ctx.type().text

        log(""" || Return type: $returnTypeName""")

        val t = TypeParser.parse(symbolTable, ctx.type())
        val f = symbolTable.lookup(funcName)

        when {
            t !is ReturnableType -> {
                errors.addError(NotReturnableError(ctx.type().start, t))
            }
            f != null -> {
                errors.addError(FunctionAlreadyDeclaredError(ident.start, funcName))
            }
        }

        val func = FunctionDeclarationAST(scopeAST, symbolTable, t, funcName)
        declaredFunctions[funcName] = func

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
            func.formals.map { p -> p.ident },
            symbolTable
        )
        parentScope.add(funcName, func.funcIdent)
        log("|| Visiting $funcName function body")
        visit(ctx.valid_return_stat())
        symbolTable = symbolTable.parentScope()!!
        scopeAST = scopeAST.parent!!

        return func
    }

    override fun visitParam(ctx: ParamContext): ASTNode {
        val type = ctx.type()
        val typeName = type.text
        val param = ctx.ident()
        val paramName = param.text

        log(
            """Visiting parameter
                || Type name: $typeName
                || Param name: $paramName
            """
        )

        val parameterAST: ParameterAST

        val t = TypeParser.parse(symbolTable, type)
        val p = symbolTable.lookup(paramName)

        when {
            t !is ReturnableType -> {
                errors.addError(CannotBeParameterError(type.start, t))
            }
            p != null -> {
                if (p is Param) {
                    errors.addError(ParameterAlreadyDeclaredError(param.start, paramName))
                }
            }
        }

        parameterAST = ParameterAST(symbolTable, paramName, Param(t as ReturnableType))

        symbolTable.add(paramName, parameterAST.ident)

        return parameterAST
    }

    override fun visitIfStat(ctx: IfStatContext): ASTNode {
        log("Visiting if statement")

        log("Visiting if statement condition expression")
        val condExpr = visit(ctx.expr()) as ExpressionAST

        if (condExpr.type != BasicType.BoolType) {
            errors.addError(CondTypeError(ctx.expr().start, condExpr.type))
        }

        val ifBlock = IfBlockAST(scopeAST, symbolTable, condExpr)

        scopeAST = ifBlock
        symbolTable = symbolTable.subScope()
        log("|| Visiting then block")
        val thenStat = visit(ctx.stat(0)) as StatementAST
        symbolTable = symbolTable.parentScope()!!
        scopeAST = scopeAST.parent!!

        val elseBlock = ElseBlockAST(ifBlock, symbolTable)

        scopeAST = elseBlock
        symbolTable = symbolTable.subScope()
        log("|| Visiting else block")
        val elseStat = visit(ctx.stat(1)) as StatementAST
        symbolTable = symbolTable.parentScope()!!
        scopeAST = scopeAST.parent!!

        ifBlock.elseBlock = elseBlock

        return addToScope(ifBlock)
    }

    override fun visitWhileStat(ctx: WhileStatContext): ASTNode {
        log("Visiting while statement")

        log("|| Visiting while condition expression")
        val condExpr = visit(ctx.expr()) as ExpressionAST

        if (condExpr.type != BasicType.BoolType) {
            errors.addError(CondTypeError(ctx.expr().start, condExpr.type))
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

    override fun visitPrintStat(ctx: PrintStatContext): ASTNode {
        log("Visiting print statement")

        val expr = visit(ctx.expr()) as ExpressionAST

        return addToScope(PrintStatementAST(scopeAST, symbolTable, expr))
    }

    override fun visitPrintlnStat(ctx: PrintlnStatContext): ASTNode {
        log("Visiting println statement")

        val expr = visit(ctx.expr()) as ExpressionAST

        return addToScope(PrintlnStatementAST(scopeAST, symbolTable, expr))
    }

    override fun visitExitStat(ctx: ExitStatContext): ASTNode {
        log("Visiting exit statement")

        val expr = visit(ctx.expr()) as ExpressionAST
        if (expr.type != IntType) {
            errors.addError(ExitTypeError(ctx.expr().start, expr.type))
        }

        return addToScope(ExitStatementAST(scopeAST, expr))
    }

    override fun visitReturn_stat(ctx: Return_statContext): ASTNode? {
        log("Visiting return statement")

        var enclosingAST: BlockAST? = scopeAST

        while (enclosingAST != null && enclosingAST !is FunctionDeclarationAST) {
            enclosingAST = enclosingAST.parent
        }

        if (enclosingAST == null) {
            errors.addError(IllegalReturnStatementError(ctx.start))
            return null
        }

        val func = enclosingAST as FunctionDeclarationAST
        val returnType = func.returnType

        log(" || return statement is under function ${func.funcName}")

        val expr = visit(ctx.expr()) as ExpressionAST

        log("we're in return stat")
        log("asserted return type: $returnType")
        log("actual return type: ${expr.type}")

        when {
            ctx.RETURN() != null && !returnType.compatible(expr.type) -> {
                errors.addError(ReturnTypeError(ctx.expr().start, returnType, expr.type))
            }
        }

        val returnStat = ReturnStatementAST(func, symbolTable, expr)
        func.returnStat = returnStat

        return addToScope(returnStat)
    }

    override fun visitSequenceRecursiveReturn1(ctx: WaccParser
    .SequenceRecursiveReturn1Context): ASTNode? {
        addToScope(visit(ctx.return_stat()) as StatementAST)
        if (ctx.valid_return_stat() != null) {
            visit(ctx.valid_return_stat())
        }
        return null
    }

    override fun visitSequenceRecursiveReturn2(ctx: WaccParser
    .SequenceRecursiveReturn2Context): ASTNode? {
        addToScope(visit(ctx.stat()) as StatementAST)
        visit(ctx.valid_return_stat())
        return null
    }

    override fun visitBeginEndReturn(ctx: WaccParser.BeginEndReturnContext):
            ASTNode? {
        symbolTable = symbolTable.subScope()
        val node = visit(ctx.valid_return_stat())
        symbolTable = symbolTable.parentScope()!!

        return node
    }

    override fun visitIfReturn(ctx: WaccParser.IfReturnContext): ASTNode {
        log("Visiting if statement")

        log("Visiting if statement condition expression")
        val condExpr = visit(ctx.expr()) as ExpressionAST

        if (condExpr.type != BasicType.BoolType) {
            errors.addError(CondTypeError(ctx.expr().start, condExpr.type))
        }

        val ifBlock = IfBlockAST(scopeAST, symbolTable, condExpr)

        scopeAST = ifBlock
        symbolTable = symbolTable.subScope()
        log("|| Visiting then block")
        visit(ctx.valid_return_stat(0))
        symbolTable = symbolTable.parentScope()!!
        scopeAST = scopeAST.parent!!

        val elseBlock = ElseBlockAST(ifBlock, symbolTable)

        scopeAST = elseBlock
        symbolTable = symbolTable.subScope()
        log("|| Visiting else block")
        visit(ctx.valid_return_stat(1))
        symbolTable = symbolTable.parentScope()!!
        scopeAST = scopeAST.parent!!

        ifBlock.elseBlock = elseBlock

        return addToScope(ifBlock)
    }

    override fun visitFreeStat(ctx: FreeStatContext): ASTNode {
        log("Visiting free statement")

        val ctxExpr = ctx.expr()
        val expr = visit(ctxExpr) as ExpressionAST
        if (expr.type !is HeapAllocatedType) {
            errors.addError(FreeTypeError(ctxExpr.start, expr.type))
        }

        return addToScope(FreeStatementAST(scopeAST, symbolTable, expr))
    }

    override fun visitSkipStat(ctx: SkipStatContext?): ASTNode {
        log("Visiting skip statement")

        return addToScope(SkipStatementAST(scopeAST))
    }

    override fun visitReadStat(ctx: ReadStatContext): ASTNode {
        log("Visiting read statement")

        val assignLhs = ctx.assign_lhs()
        val target = visit(assignLhs) as AssignmentAST<*>
        if (target.type !is BasicType || target.type == BasicType.BoolType) {
            errors.addError(ReadTypeError(assignLhs.start, target.type))
        }

        log("|| Target type: ${target.type}")

        return addToScope(ReadStatementAST(scopeAST, symbolTable, target))
    }

    //endregion

    //region assign_and_declare

    override fun visitDeclarationStat(ctx: DeclarationStatContext): ASTNode {
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

        val exprRhs = ctx.assign_rhs()
        val assignRhs = visit(exprRhs) as AssignRhsAST

        when {
            v != null -> {
                if (v !is FunctionType) {
                    errors.addError(IdentifierAlreadyDeclaredError(ident.start, varName))
                }
            }
            !t.compatible(assignRhs.type) -> {
                errors.addError(AssignTypeError(exprRhs.start, t, assignRhs.type))
            }
        }

        val varDecl = VariableDeclarationAST(
            scopeAST,
            symbolTable,
            typeName,
            assignRhs,
            Variable(t as ReturnableType)
        )
        symbolTable.add(varName, varDecl.varIdent)

        return addToScope(varDecl)
    }

    override fun visitAssignmentStat(ctx: AssignmentStatContext): ASTNode {
        val exprRhs = ctx.assign_rhs()
        val assignRhs = visit(exprRhs) as AssignRhsAST
        val assignLhs = visit(ctx.assign_lhs()) as AssignmentAST<*>

        log("Visiting variable assignment ${ctx.assign_lhs().text}")

        if (!assignLhs.type.compatible(assignRhs.type)) {
            errors.addError(AssignTypeError(exprRhs.start, assignLhs.type, assignRhs.type))
        }

        println("assignRhs: $assignRhs")

        assignLhs.rhs = assignRhs

        return addToScope(assignLhs)
    }

    override fun visitIdentAssignLhs(ctx: IdentAssignLhsContext): ASTNode {
        val ident = visitIdent(ctx.ident()) as VariableIdentifierAST

        return AssignToIdentAST(scopeAST, ident)
    }

    override fun visitExprAssignRhs(ctx: ExprAssignRhsContext): ASTNode {
        return visit(ctx.expr())
    }

    override fun visitArrayLiterAssignRhs(ctx: ArrayLiterAssignRhsContext): ASTNode {
        return visit(ctx.array_liter())
    }

    override fun visitPairElemAssignRhs(ctx: PairElemAssignRhsContext): ASTNode {
        return visit(ctx.pair_elem())
    }

    override fun visitArrayElemAssignLhs(ctx: ArrayElemAssignLhsContext): ASTNode {
        return AssignToArrayElemAST(scopeAST, visitArray_elem(ctx.array_elem()) as ArrayElemAST)
    }

    override fun visitPairElemAssignLhs(ctx: PairElemAssignLhsContext): ASTNode {
        return AssignToPairElemAST(scopeAST, visit(ctx.pair_elem()) as PairElemAST)
    }

    override fun visitArray_elem(ctx: Array_elemContext): ASTNode {
        val identCtx = ctx.ident()
        val ident = visitIdent(identCtx) as VariableIdentifierAST
        val arr: ArrayType
        if (ident.type !is ArrayType) {
            errors.addError(IndexingNonArrayTypeError(identCtx.start, ident.type))
            arr = ArrayType.ANY_ARRAY
        } else {
            arr = (symbolTable.lookupAll(ident.varName) as Variable).type as ArrayType
        }
        val indexList = mutableListOf<ExpressionAST>()

        for (expr in ctx.expr()) {
            val indexExpr = visit(expr) as ExpressionAST
            if (indexExpr.type != IntType) {
                errors.addError(ArrayIndexTypeError(expr.start, indexExpr.type))
            }
            indexList.add(indexExpr)
        }

        return ArrayElemAST(
            symbolTable, ctx.ident().text, indexList,
            arr.elementType as ReturnableType
        )
    }

    override fun visitNewPairAssignRhs(ctx: NewPairAssignRhsContext): ASTNode {
        val expr1 = visit(ctx.expr(0)) as ExpressionAST
        val expr2 = visit(ctx.expr(1)) as ExpressionAST
        return NewPairAST(symbolTable, expr1, expr2)
    }

    override fun visitFstPair(ctx: FstPairContext): ASTNode {
        val exprCtx = ctx.expr()
        val expr = visit(exprCtx) as ExpressionAST
        if (expr.type !is PairType) {
            errors.addError(FstTypeError(exprCtx.start, expr.type))
        }

        return FstPairElemAST(symbolTable, expr)
    }

    override fun visitSndPair(ctx: SndPairContext): ASTNode {
        val exprCtx = ctx.expr()
        val expr = visit(exprCtx) as ExpressionAST
        if (expr.type !is PairType) {
            errors.addError(SndTypeError(exprCtx.start, expr.type))
        }

        return SndPairElemAST(symbolTable, expr)
    }

    override fun visitCallAssignRhs(ctx: CallAssignRhsContext): ASTNode {
        val ident = ctx.ident()
        val funcName = ident.text
        log("Visiting $funcName function call")
        var f = symbolTable.lookupAll(funcName)

        val args: MutableList<ExprContext> =
            if (ctx.arg_list() != null) ctx.arg_list().expr() else mutableListOf()

        if (f == null) {
            if (functionsToVisit.containsKey(funcName)) {
                log("Function $funcName called before it was defined!")
                val oldAst = scopeAST
                val oldSt = symbolTable
                // Go back to the top scope as that is the only place where functions can be
                // defined
                scopeAST = topAst
                symbolTable = topSymbolTable
                f = (visitFunc(functionsToVisit[funcName]!!) as FunctionDeclarationAST).funcIdent
                scopeAST = oldAst
                symbolTable = oldSt
                functionsToVisit.remove(funcName)
            } else {
                errors.addError(IdentifierNotDefinedError(ident.start, funcName))
            }
        }

        if (f !is FunctionType) {
            errors.addError(NotAFunctionError(ident.start, funcName))
        }
        f = f as FunctionType

        val actuals: MutableList<ExpressionAST> = LinkedList()

        if (f.formals.size != args.size) {
            errors.addError(
                NumArgumentsError(ctx.arg_list().start, funcName, f.formals.size, args.size)
            )
        }

        for (k in args.indices) {
            if (k >= f.formals.size) {
                break
            }
            val arg = args[k]
            val argExpr = visit(arg) as ExpressionAST
            if (!f.formals[k].type.compatible(argExpr.type)) {
                errors.addError(
                    ParameterTypeError(arg.start, funcName, k, f.formals[k].type, argExpr.type)
                )
            }
            actuals.add(argExpr)
        }

        return CallAST(symbolTable, funcName, f, actuals)
    }

    //endregion

    //region literals

    override fun visitIdent(ctx: IdentContext): ASTNode {
        log("Visiting identifier with name ${ctx.text}")
        var ident = symbolTable.lookupAll(ctx.text)
        if (ident == null) {
            errors.addError(IdentifierNotDefinedError(ctx.start, ctx.text))
            ident = Variable.ANY_VAR
        } else if (ident !is Variable) {
            errors.addError(IdentifierNotAVariableError(ctx.start, ctx.text))
            ident = Variable.ANY_VAR
        }
        log("Identifier is variable with type ${ident.type}")
        return VariableIdentifierAST(symbolTable, ctx.text, ident)
    }

    override fun visitInt_liter(ctx: Int_literContext): ASTNode {
        return visit(ctx.children[0])
    }

    override fun visitInt_liter_positive(ctx: Int_liter_positiveContext): ASTNode {
        val i = Integer.parseInt(ctx.POSITIVE_OR_NEGATIVE_INTEGER().text)
        assert(i in 0..INT_MAX)

        return IntLiteralAST(i)
    }

    override fun visitInt_liter_negative(ctx: Int_liter_negativeContext): ASTNode {
        val i = Integer.parseInt(ctx.text)
        assert(i in INT_MIN..0)

        return IntLiteralAST(i)
    }

    override fun visitTBool(ctx: TBoolContext?): ASTNode {
        return BoolLiteralAST(true)
    }

    override fun visitFBool(ctx: FBoolContext?): ASTNode {
        return BoolLiteralAST(false)
    }

    override fun visitChar_liter(ctx: Char_literContext): ASTNode {
        return CharLiteralAST(ctx.text.substring(1, ctx.text.length - 1)[0])
    }

    override fun visitStr_liter(ctx: Str_literContext): ASTNode {
        return StringLiteralAST(ctx.text.trim('\"'))
    }

    override fun visitPair_liter(ctx: Pair_literContext): ASTNode {
        return NullPairLiteralAST()
    }

    override fun visitArray_liter(ctx: Array_literContext): ASTNode {

        val elems: MutableList<ExpressionAST> = LinkedList()

        var elemType = Type.ANY
        for (expr in ctx.expr()) {
            val elem = visit(expr) as ExpressionAST
            if (!elemType.compatible(elem.type)) {
                errors.addError(ArrayLiteralElemsTypeError(expr.start, elemType, elem.type))
            }
            elemType = elem.type
            elems.add(elem)
        }

        return ArrayLiteralAST(symbolTable, elemType, elems)
    }

    override fun visitUnaryExpr(ctx: UnaryExprContext): ASTNode {
        log("Visiting unary operator expression")

        val unOp: UnaryOp = when {
            ctx.BANG() != null -> UnaryOp.BANG
            ctx.MINUS() != null -> UnaryOp.MINUS
            ctx.LEN() != null -> UnaryOp.LEN
            ctx.ORD() != null -> UnaryOp.ORD
            ctx.CHR() != null -> UnaryOp.CHR
            else -> null
        }!!
        log("Found unary operator $unOp")

        val expr = visit(ctx.expr()) as ExpressionAST

        if (!unOp.expectedType.compatible(expr.type)) {
            errors.addError(UnaryOpTypeError(ctx.start, unOp, expr.type))
        }

        return UnaryOpExprAST(symbolTable, expr, unOp)
    }

    override fun visitBinaryExpr(ctx: BinaryExprContext): ASTNode {
        log("Visiting binary operator expression")
        val binOp: BinaryOp = when {
            ctx.MULT() != null -> BinaryOp.MULT
            ctx.DIV() != null -> BinaryOp.DIV
            ctx.MOD() != null -> BinaryOp.MOD
            ctx.PLUS() != null -> BinaryOp.PLUS
            ctx.MINUS() != null -> BinaryOp.MINUS
            ctx.GT() != null -> BinaryOp.GT
            ctx.GTE() != null -> BinaryOp.GTE
            ctx.LT() != null -> BinaryOp.LT
            ctx.LTE() != null -> BinaryOp.LTE
            ctx.EQUALS() != null -> BinaryOp.EQUALS
            ctx.NOT_EQUALS() != null -> BinaryOp.NOT_EQUALS
            ctx.AND() != null -> BinaryOp.AND
            ctx.OR() != null -> BinaryOp.OR
            else -> null
        }!!
        log("Found binary operator $binOp")
        val expr1 = visit(ctx.expr(0)) as ExpressionAST
        val expr2 = visit(ctx.expr(1)) as ExpressionAST

        if (binOp.allowedTypes != null) {
            if (!binOp.allowedTypes.contains(expr1.type)) {
                errors.addError(BinaryOpTypeError(ctx.start, binOp, expr1.type))
            }
            if (!binOp.allowedTypes.contains(expr1.type)) {
                errors.addError(BinaryOpTypeError(ctx.start, binOp, expr1.type))
            }
        }

        if (!expr1.type.compatible(expr2.type)) {
            errors.addError(BinaryOpDifferentTypesError(ctx.start, expr1.type, expr2.type))
        }

//        if (arrayOf(BinaryOp.AND, BinaryOp.OR).any { it == node.operator }) {
//            if (!node.expr1.type.compatible(BasicType.BoolType) ||
//                !node.expr2.type.compatible(BasicType.BoolType)
//            ) {
//                throw TypeError(
//                    "boolean operands expected in expression but " +
//                        "found operands of type ${node.expr1.type} and ${node.expr2.type} instead"
//                )
//            }
//        }
//
//        if (!node.expr1.type.compatible(node.expr2.type)) {
//            throw TypeError(
//                "left operand of type ${node.expr1.type} is incompatible " +
//                    "with right operand of type ${node.expr2.type}"
//            )
//        }

        return BinaryOpExprAST(symbolTable, expr1, expr2, binOp)
    }

    override fun visitBracketExpr(ctx: BracketExprContext): ASTNode {
        return visit(ctx.expr())
    }

    //endregion

    //region helpers

    private fun addToScope(stat: StatementAST): StatementAST {
        scopeAST.statements.add(stat)
        return stat
    }

    private fun log(message: String) {
        if (enableLogging) {
            println(message.trimMargin())
        }
    }

    //endregion
}
