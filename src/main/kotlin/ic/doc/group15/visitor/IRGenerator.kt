package ic.doc.group15.visitor

import ic.doc.group15.assembly.TranslatorMethod
import ic.doc.group15.ast.*

class IRGenerator {
    companion object {
        private val translators: ASTTranslatorMap = generateASTTranslatorMap(IRGenerator::class)
    }

    private fun translate(node: ASTNode) {
        IRGenerator.translators[node::class]?.call(this, node)
    }

    @TranslatorMethod(AST::class)
    private fun translateProgram(program: AST) {
        TODO()
    }

    @TranslatorMethod(FunctionDeclarationAST::class)
    private fun translateFunctionDeclaration(node: FunctionDeclarationAST) {
        TODO()
    }


    @TranslatorMethod(IfBlockAST::class)
    private fun translateIfBlock(stat: IfBlockAST) {
        TODO()
    }

    @TranslatorMethod(WhileBlockAST::class)
    private fun translateWhileBlock(node: WhileBlockAST) {
        TODO()
    }

    @TranslatorMethod(BeginEndBlockAST::class)
    private fun translateBeginEndBlock(node: BeginEndBlockAST) {
        TODO()
    }

    @TranslatorMethod(CallAST::class)
    private fun translateFunctionCall(node: CallAST) {
        TODO()
    }

    @TranslatorMethod(VariableDeclarationAST::class)
    private fun translateVariableDeclaration(node: VariableDeclarationAST) {
        TODO()
    }

    @TranslatorMethod(AssignToIdentAST::class)
    private fun translateAssignToVariable(node: AssignToIdentAST) {
        TODO()
    }

    @TranslatorMethod(FreeStatementAST::class)
    private fun translateFreeStat(node: FreeStatementAST) {
        TODO()
    }

    @TranslatorMethod(ReturnStatementAST::class)
    private fun translateReturnStat(node: ReturnStatementAST) {
        TODO()
    }

    @TranslatorMethod(ExitStatementAST::class)
    private fun translateExitStat(node: ExitStatementAST) {
        TODO()
    }

    @TranslatorMethod(PrintStatementAST::class)
    private fun translatePrintStat(node: PrintStatementAST) {
        TODO()
    }

    @TranslatorMethod(PrintlnStatementAST::class)
    private fun translatePrintlnStat(node: PrintlnStatementAST) {
        TODO()
    }

    @TranslatorMethod(ReadStatementAST::class)
    private fun translateReadStat(node: ReadStatementAST) {
        TODO()
    }

    @TranslatorMethod(NewPairAST::class)
    private fun translateNewPair(node: NewPairAST) {
        TODO()
    }

    @TranslatorMethod(IntLiteralAST::class)
    private fun translateIntLiteral(node: IntLiteralAST) {
        TODO()
    }

    @TranslatorMethod(BoolLiteralAST::class)
    private fun translateBoolLiteral(node: BoolLiteralAST) {
        TODO()
    }

    @TranslatorMethod(CharLiteralAST::class)
    private fun translateCharLiteral(node: CharLiteralAST) {
        TODO()
    }

    @TranslatorMethod(StringLiteralAST::class)
    private fun translateStringLiteral(node: StringLiteralAST) {
        TODO()
    }

    @TranslatorMethod(VariableIdentifierAST::class)
    private fun translateVariableIdentifier(node: VariableIdentifierAST) {
        TODO()
    }

    @TranslatorMethod(NullPairLiteralAST::class)
    @Suppress("UNUSED_PARAMETER")
    private fun translateNullPairLiteralAST(node: NullPairLiteralAST) {
        TODO()
    }

    @TranslatorMethod(ArrayLiteralAST::class)
    private fun translateArrayLiteral(node: ArrayLiteralAST) {
        TODO()
    }

    @TranslatorMethod(ArrayElemAST::class)
    private fun translateArrayElem(node: ArrayElemAST) {
        TODO()
    }

    @TranslatorMethod(AssignToArrayElemAST::class)
    private fun translateAssignToArrayElem(node: AssignToArrayElemAST) {
        TODO()
    }

    @TranslatorMethod(FstPairElemAST::class)
    private fun translateFstPairElem(node: FstPairElemAST) {
        TODO()
    }

    @TranslatorMethod(SndPairElemAST::class)
    private fun translateSndPairElem(node: SndPairElemAST) {
        TODO()
    }

    @TranslatorMethod(AssignToPairElemAST::class)
    private fun translateAssignToPairElem(node: AssignToPairElemAST) {
        TODO()
    }

    @TranslatorMethod(UnaryOpExprAST::class)
    private fun translateUnOp(unOpExpr: UnaryOpExprAST) {
        TODO()
    }

    @TranslatorMethod(BinaryOpExprAST::class)
    private fun translateBinOp(expr: BinaryOpExprAST) {
        TODO()
    }
}