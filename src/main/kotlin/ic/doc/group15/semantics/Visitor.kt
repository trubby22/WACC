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

                st = st.subScope()
                ast = func
                visitParam_list(ctx.param_list())
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

    override fun visitParam_list(ctx: WaccParser.Param_listContext): ASTNode? {
        if (ast !is FunctionDeclarationAST) {
            throw DeclarationError("params must be declared in a function definition")
        }

        log(
            """Visiting parameters of function ${(ast as FunctionDeclarationAST).funcName}"""
        )

        for (param in ctx.param()) {
            visitParam(param)
        }
        return null
    }

    override fun visitParam(ctx: WaccParser.ParamContext): ASTNode? {
        assert(ast is FunctionDeclarationAST)

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
        (ast as FunctionDeclarationAST).formals.add(parameterAST)

        return visitChildren(ctx)
    }

    override fun visitDeclarationStat(ctx: WaccParser.DeclarationStatContext): ASTNode? {
        // TODO: handle all assign_rhs cases

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
                throw DeclarationError("$varName is already declared")
            }
            else -> {
                varDecl.varIdent = Variable(t)
            }
        }

        st.add(varName, varDecl.varIdent)

        return visitChildren(ctx)
    }

    override fun visitAssignmentStat(ctx: WaccParser.AssignmentStatContext): ASTNode? {
        // TODO
        /*
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
        */
        return visitChildren(ctx)
    }
}
