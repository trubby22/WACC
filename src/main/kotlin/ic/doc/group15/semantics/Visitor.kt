package ic.doc.group15.semantics

import ic.doc.group15.antlr.WaccParser
import ic.doc.group15.antlr.WaccParserBaseVisitor
import java.util.logging.Level
import java.util.logging.Logger

class Visitor(
    private val abstractSyntaxTree: AST,
    private val symbolTable: SymbolTable
) : WaccParserBaseVisitor<ASTNode>() {

    private var ast: ASTNode = abstractSyntaxTree
    private var st = symbolTable

    companion object {
        private val LOG = Logger.getLogger(Visitor::class.java.name)

        private fun log(message: String) {
            LOG.log(Level.FINE, message.trimMargin())
        }
    }

    override fun visitProgram(ctx: WaccParser.ProgramContext): ASTNode {
        if (!st.isTopLevel()) {
            throw DeclarationError("begin/end not allowed in this scope")
        }

        log("Visiting program")

        val program = ast as AST

        log("Visiting function definitions")

        for (func in ctx.func()) {
            program.functions.add(visitFunc(func) as FunctionDeclarationAST)
        }

        program.main = visit(ctx.stat()) as StatementAST

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

        if (!st.isTopLevel()) {
            throw DeclarationError("functions cannot be declared in this scope")
        }

        val t = st.lookupAll(returnTypeName)
        val f = st.lookup(funcName)

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
                    ast, st, returnTypeName, funcName
                )

                log(
                    """Visiting parameters of function ${func.funcName}"""
                )

                st = st.subScope()
                ast = func

                for (param in ctx.param()) {
                    func.formals.add(visitParam(param) as ParameterAST)
                }

                st = st.parentScope()!!
                ast = ast.parent!!

                func.funcIdent = FunctionType(
                    t,
                    func.formals.map { p -> p.paramIdent },
                    st
                )

                symbolTable.add(funcName, func.funcIdent)

                val body = visit(ctx.stat())
                if (body !is StatementAST) {
                    throw DeclarationError("invalid function body in function $funcName")
                }
                func.body = body

                visitValid_return_stat(ctx.valid_return_stat())
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

        val parameterAST = ParameterAST(ast, st, typeName, paramName)

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

        val varDecl = VariableDeclarationAST(ast, st, typeName, varName)

        val t = st.lookupAll(typeName)
        val v = st.lookup(varName)

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

        st.add(varName, varDecl.varIdent)

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
}
