package ic.doc.group15.visitor

import ic.doc.group15.assembly.TranslatorMethod
import ic.doc.group15.ast.*
import ic.doc.group15.ssa.Function
import ic.doc.group15.ssa.State
import ic.doc.group15.type.BasicType

class IRGenerator(private val enableLogging: Boolean = true) {
    companion object {
        private val translators: ASTTranslatorMap = generateASTTranslatorMap(IRGenerator::class)
    }

    private fun translate(state: State, node: ASTNode): Any? {
        return translators[node::class]?.call(this, state, node)
    }

    @TranslatorMethod(AST::class)
    private fun translateProgram(state: State, program: AST): List<Function> {
        log("Building SSA for program")
        val (functions, statements) = program.statements.partition { node -> node is FunctionDeclarationAST }
        // Build SSA form for each function TODO(figure out what state to store or throw away entirely)
        val ssaFunctions = mutableListOf<Function>()

        functions.forEach { ssaFunctions.add(translate(State(), it) as Function) }

        val ssaMainFunction = translateMain(State(), program, *statements.toTypedArray())
        ssaFunctions.add(ssaMainFunction)
        return ssaFunctions
    }

    private fun translateMain(state: State, program: AST, vararg stat: StatementAST): Function {
        val node = FunctionDeclarationAST(program,
            program.symbolTable,
            program.symbolTable.subScope(),
            BasicType.IntType,
            "main"
        )
        val statements = node.statements
        // Add all statements
        statements.addAll(stat)
        // Add return statement if not found
        if (statements.isEmpty()
            || statements.last() !is ReturnStatementAST
            || statements.last() !is ExitStatementAST) {
            val returnNode = ReturnStatementAST(node, node.symbolTable, IntLiteralAST(0))
            statements.add(returnNode)
        }

        return translate(state, node) as Function
    }

    /**
     * 1) Build CFG for each function
     * 2) Construct dominator tree and compute set of dominance frontier for each basic block
     * 3) Insert phi nodes
     * 4) Rename variables to obtain SSA form
     **/
    @TranslatorMethod(FunctionDeclarationAST::class)
    private fun translateFunctionDeclaration(state: State, node: FunctionDeclarationAST) {
        TODO()
    }


    @TranslatorMethod(IfBlockAST::class)
    private fun translateIfBlock(state: State, node: IfBlockAST) {
        TODO()
    }

    @TranslatorMethod(WhileBlockAST::class)
    private fun translateWhileBlock(state: State, node: WhileBlockAST) {
        TODO()
    }

    @TranslatorMethod(BeginEndBlockAST::class)
    private fun translateBeginEndBlock(state: State, node: BeginEndBlockAST) {
        TODO()
    }

    @TranslatorMethod(CallAST::class)
    private fun translateFunctionCall(state: State, node: CallAST) {
        TODO()
    }

    @TranslatorMethod(VariableDeclarationAST::class)
    private fun translateVariableDeclaration(state: State, node: VariableDeclarationAST) {
        TODO()
    }

    @TranslatorMethod(AssignToIdentAST::class)
    private fun translateAssignToVariable(state: State, node: AssignToIdentAST) {
        TODO()
    }

    @TranslatorMethod(FreeStatementAST::class)
    private fun translateFreeStat(state: State, node: FreeStatementAST) {
        TODO()
    }

    @TranslatorMethod(ReturnStatementAST::class)
    private fun translateReturnStat(state: State, node: ReturnStatementAST) {
        TODO()
    }

    @TranslatorMethod(ExitStatementAST::class)
    private fun translateExitStat(state: State, node: ExitStatementAST) {
        TODO()
    }

    @TranslatorMethod(PrintStatementAST::class)
    private fun translatePrintStat(state: State, node: PrintStatementAST) {
        TODO()
    }

    @TranslatorMethod(PrintlnStatementAST::class)
    private fun translatePrintlnStat(state: State, node: PrintlnStatementAST) {
        TODO()
    }

    @TranslatorMethod(ReadStatementAST::class)
    private fun translateReadStat(state: State, node: ReadStatementAST) {
        TODO()
    }

    @TranslatorMethod(NewPairAST::class)
    private fun translateNewPair(state: State, node: NewPairAST) {
        TODO()
    }

    @TranslatorMethod(IntLiteralAST::class)
    private fun translateIntLiteral(state: State, node: IntLiteralAST) {
        TODO()
    }

    @TranslatorMethod(BoolLiteralAST::class)
    private fun translateBoolLiteral(state: State, node: BoolLiteralAST) {
        TODO()
    }

    @TranslatorMethod(CharLiteralAST::class)
    private fun translateCharLiteral(state: State, node: CharLiteralAST) {
        TODO()
    }

    @TranslatorMethod(StringLiteralAST::class)
    private fun translateStringLiteral(state: State, node: StringLiteralAST) {
        TODO()
    }

    @TranslatorMethod(VariableIdentifierAST::class)
    private fun translateVariableIdentifier(state: State, node: VariableIdentifierAST) {
        TODO()
    }

    @TranslatorMethod(NullPairLiteralAST::class)
    @Suppress("UNUSED_PARAMETER")
    private fun translateNullPairLiteralAST(state: State, node: NullPairLiteralAST) {
        TODO()
    }

    @TranslatorMethod(ArrayLiteralAST::class)
    private fun translateArrayLiteral(state: State, node: ArrayLiteralAST) {
        TODO()
    }

    @TranslatorMethod(ArrayElemAST::class)
    private fun translateArrayElem(state: State, node: ArrayElemAST) {
        TODO()
    }

    @TranslatorMethod(AssignToArrayElemAST::class)
    private fun translateAssignToArrayElem(state: State, node: AssignToArrayElemAST) {
        TODO()
    }

    @TranslatorMethod(FstPairElemAST::class)
    private fun translateFstPairElem(state: State, node: FstPairElemAST) {
        TODO()
    }

    @TranslatorMethod(SndPairElemAST::class)
    private fun translateSndPairElem(state: State, node: SndPairElemAST) {
        TODO()
    }

    @TranslatorMethod(AssignToPairElemAST::class)
    private fun translateAssignToPairElem(state: State, node: AssignToPairElemAST) {
        TODO()
    }

    @TranslatorMethod(UnaryOpExprAST::class)
    private fun translateUnOp(state: State, node: UnaryOpExprAST) {
        TODO()
    }

    @TranslatorMethod(BinaryOpExprAST::class)
    private fun translateBinOp(state: State, node: BinaryOpExprAST) {
        TODO()
    }

    private fun log(str: String) {
        if (enableLogging) {
            println(str)
        }
    }
}