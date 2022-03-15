package ic.doc.group15.ssa.cfg

import ic.doc.group15.ast.*
import ic.doc.group15.ssa.BasicBlock
import ic.doc.group15.ssa.IRFunction
import ic.doc.group15.ssa.tac.*
import ic.doc.group15.translator.AssemblyGenerator
import ic.doc.group15.translator.TranslatorMethod
import ic.doc.group15.type.BasicType.*

/**
 * Convert the AST representation of the program to three-address code and build a control flow
 * graph for each function.
 *
 * Assumption: All nodes (read: basic blocks) can have at most two successors
 */
class CfgGenerator(
    ast: AST,
    enableLogging: Boolean = true
) : AssemblyGenerator<ASTNode>(ast, enableLogging) {

    @TranslatorMethod
    private fun buildCFGFromProgram(program: AST): List<IRFunction> {
        log("Building CFG for program")
        val (functions, statements) = program.statements.partition { it is FunctionDeclarationAST }

        // Build SSA form for each function
        val ssaIRFunctions = mutableListOf<IRFunction>()

        // Note that each function state is independent of each other
        // TODO introduce concurrency
        for (funcNode in functions) {
            val funcState = CfgState()
            buildCFGFromFunctionDeclaration(funcNode as FunctionDeclarationAST, funcState)
            ssaIRFunctions.add(funcState.irFunction)
        }

        // Construct main function separately (due to WACC language design constraints)
        val mainState = CfgState()
        buildCFGFromMain(mainState, program, *statements.toTypedArray())
        ssaIRFunctions.add(mainState.irFunction)

        return ssaIRFunctions
    }

    private fun buildCFGFromMain(cfgState: CfgState, program: AST, vararg stat: StatementAST) {
        val mainNode = FunctionDeclarationAST(
            program,
            program.symbolTable,
            program.symbolTable.subScope(),
            IntType,
            "main"
        )
        val statements = mainNode.statements
        // Add all statements
        statements.addAll(stat)
        // Add return statement if not found
        if (statements.isEmpty() ||
            statements.last() !is ReturnStatementAST ||
            statements.last() !is ExitStatementAST) {
            val returnNode = ReturnStatementAST(mainNode, mainNode.symbolTable, IntLiteralAST(0))
            statements.add(returnNode)
        }

        buildCFGFromFunctionDeclaration(mainNode, cfgState)
    }

    /**
     * Populate the state with relevant information and build CFG for the function.
     * 1) Build CFG for each function
     * 2) Construct dominator tree and compute set of dominance frontier for each basic block
     * 3) Insert phi nodes
     * 4) Rename variables to obtain SSA form
     **/
    @TranslatorMethod
    private fun buildCFGFromFunctionDeclaration(node: FunctionDeclarationAST, cfgState: CfgState) {
        log("Building CFG for function ${node.funcName}")
        // Build CFG for the function
        initialiseState(node, cfgState)
        node.statements.forEach { translate(it, cfgState) }

        // Form an edge between last instruction and function exit block
        cfgState.irFunction.sealBlock()
    }

    /**
     *                        if entry
     *                 (initial current block)
     *                        -      -
     *                       -        -
     *                      -          -
     *                    <-            ->
     *                 then block     else block
     *                     -             -
     *                      -           -
     *                       -         -
     *                        ->     <-
     *                         if exit
     *                       (join node)
     *
     *  In linear representation: cond block -> else block -> then block -> exit block
     **/
    @TranslatorMethod
    private fun buildCFGFromIfBlock(node: IfBlockAST, cfgState: CfgState) {
        log("Building CFG for if block")
        val thenBlock = BasicBlock(cfgState.irFunction)
        val elseBlock = BasicBlock(cfgState.irFunction)
        val exitBlock = BasicBlock(cfgState.irFunction)

        // add successors to entry block
        cfgState.currentBlock.addSuccessors(thenBlock, elseBlock)

        // translate cond expr and store result in register
        translate(node.condExpr, cfgState)
        val resultReg = cfgState.resultRegister

        // add branch instruction to entry block
        val branchIfInst = BranchIf(resultReg, thenBlock)
        cfgState.currentBlock.addInstructions(branchIfInst)

        // construct then block
        log("Building CFG for then block")
        cfgState.currentBlock = thenBlock
        node.statements.forEach { translate(it, cfgState) }
        if (!node.elseBlock.statements.any { it is ReturnStatementAST || it is ExitStatementAST }) {
            cfgState.currentBlock.addSuccessors(exitBlock)
        }

        // construct else block
        log("Building CFG for else block")
        cfgState.currentBlock = elseBlock
        node.elseBlock.statements.forEach { translate(it, cfgState) }
        if (!node.elseBlock.statements.any { it is ReturnStatementAST || it is ExitStatementAST }) {
            cfgState.currentBlock.addSuccessors(exitBlock)
        }

        // Add unconditional branch statement to condition block
        val branchInst = Branch(exitBlock)
        cfgState.currentBlock.addInstructions(branchInst)

        // set current block as exit block
        cfgState.currentBlock = exitBlock
    }

    /**
     *                       while entry
     *                 (initial current block)
     *                            |
     *                            |
     *         - - - - - - - -cond block <- - -
     *         |                  |           |
     *         |                  |           |
     * (false) |             loop block       | (true)
     *         |                  |           |
     *         |                  |           |
     *         |                  - - - - - - -
     *         |
     *         |- - - - - - - - - -
     *                            |
     *                            v
     *                     end while block
     **/
    @TranslatorMethod
    private fun buildCFGFromWhileBlock(node: WhileBlockAST, cfgState: CfgState) {
        log("Building CFG for while block")
        val condBlock = BasicBlock(cfgState.irFunction)
        val loopBlock = BasicBlock(cfgState.irFunction)
        val exitBlock = BasicBlock(cfgState.irFunction)

        // add successors to current block and cond block
        cfgState.currentBlock.addSuccessors(condBlock)

        // construct cond block
        cfgState.currentBlock = condBlock

        // translate cond expr and store result in register
        translate(node.condExpr, cfgState)
        val resultReg = cfgState.resultRegister

        // Branch to exit block if boolean condition is false
        val branchIfInst = BranchIf(resultReg, exitBlock)
        cfgState.currentBlock.addInstructions(branchIfInst)

        // Add successors to current block for CFG analysis
        cfgState.currentBlock.addSuccessors(loopBlock, exitBlock)

        log("Building CFG for while loop block")
        // construct loop block
        cfgState.currentBlock = loopBlock
        node.statements.forEach { stat -> translate(stat, cfgState) }
        if (!node.statements.any { it is ReturnStatementAST || it is ExitStatementAST }) {
            cfgState.currentBlock.addSuccessors(condBlock)
        }

        // Add unconditional branch statement to condition block
        val branchInst = Branch(condBlock)
        cfgState.currentBlock.addInstructions(branchInst)

        // set current block as exit block
        cfgState.currentBlock = exitBlock
    }

    @TranslatorMethod
    private fun buildCFGFromBeginEndBlock(node: BeginEndBlockAST, cfgState: CfgState) {
        val inScopeBlock = BasicBlock(cfgState.irFunction)
        val outScopeBlock = BasicBlock(cfgState.irFunction)

        // construct in scope block
        log("Building CFG for begin end block")
        cfgState.currentBlock = inScopeBlock
        node.statements.forEach { stat -> translate(stat, cfgState) }
        if (!node.statements.any { it is ReturnStatementAST || it is ExitStatementAST }) {
            cfgState.currentBlock.addSuccessors(outScopeBlock)
        }

        // set current block as exit block
        cfgState.currentBlock = outScopeBlock
    }

    @TranslatorMethod
    private fun translateFunctionCall(node: CallAST, cfgState: CfgState) {
        val paramRegs = mutableListOf<Operand>()
        for (arg in node.actuals) {
            translate(arg, cfgState)
            // Store the result for each param to a list
           paramRegs.add(cfgState.resultRegister)
        }

        val type = node.funcIdent.returnType
        val f = CustomFunc(node.funcName, type)
        val reg = cfgState.newVar(type)
        val inst = AssignCall(reg, f, *paramRegs.toTypedArray())
        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod
    private fun translateVariableDeclaration(node: VariableDeclarationAST, cfgState: CfgState) {
        TODO()
    }

    @TranslatorMethod
    private fun translateAssignToVariable(node: AssignToIdentAST, cfgState: CfgState) {
        // Store value in result register
        translate(node.rhs, cfgState)
        // Store reference of result register in variable map
        val resultReg = cfgState.resultRegister
        cfgState.varDefinedAt[node.lhs.ident] = resultReg
    }

    @TranslatorMethod
    private fun translateFreeStat(node: FreeStatementAST, cfgState: CfgState) {
        TODO()
    }

    @TranslatorMethod
    private fun translateReturnStat(node: ReturnStatementAST, cfgState: CfgState) {
        // Translate the expression value and store it in a register
        translate(node.expr, cfgState)
        // Return value stored in latest register
        val resultReg = cfgState.resultRegister
        val inst = Call(Functions.RETURN, resultReg)
        cfgState.currentBlock.addInstructions(inst)

        // set successor to function exit point
        cfgState.currentBlock.addSuccessors(cfgState.irFunction.exitBlock)
    }

    // TODO(find a way to add an edge to end of program)
    @TranslatorMethod
    private fun translateExitStat(node: ExitStatementAST, cfgState: CfgState) {
        // Translate the expression value and store it in a register
        translate(node.expr, cfgState)
        // Exit and return exit code stored in result register
        val resultReg = cfgState.resultRegister
        val inst = Call(Functions.EXIT, resultReg)
        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod
    private fun translatePrintStat(node: PrintStatementAST, cfgState: CfgState) {
        translate(node.expr, cfgState)

        // Print value in register
        val reg = cfgState.resultRegister
        val f = Print(reg.type())
        val inst = Call(f, reg)
        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod
    private fun translatePrintlnStat(node: PrintlnStatementAST, cfgState: CfgState) {
        translate(node.expr, cfgState)

        // Print value in register
        val resultReg = cfgState.resultRegister
        val f = Println(resultReg.type())
        val inst = Call(f, resultReg)
        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod
    private fun translateReadStat(node: ReadStatementAST, cfgState: CfgState) {
        // Find if there is a previous reference
        val param: Var = when (val lhs = node.target.lhs) {
            is VariableIdentifierAST -> cfgState.varDefinedAt[lhs.ident]!!
            is ArrayElemAST -> TODO()
            is PairElemAST -> TODO()
            else -> throw IllegalArgumentException("cannot read into expression $lhs")
        }

        // Store read value in new register
        val type = node.target.type
        val f = Read(type)
        val reg = cfgState.newVar(type)
        val inst = AssignCall(reg, f, param)

        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod
    private fun translateNewPair(node: NewPairAST, cfgState: CfgState) {
        TODO()
    }

    @TranslatorMethod
    private fun translateIntLiteral(node: IntLiteralAST, cfgState: CfgState) {
        val reg = cfgState.newVar(IntType)
        val inst = AssignValue(reg, IntImm(node.intValue))

        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod
    private fun translateBoolLiteral(node: BoolLiteralAST, cfgState: CfgState) {
        val reg = cfgState.newVar(BoolType)
        val inst = AssignValue(reg, BoolImm(node.boolValue))

        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod
    private fun translateCharLiteral(node: CharLiteralAST, cfgState: CfgState) {
        val reg = cfgState.newVar(CharType)
        val inst = AssignValue(reg, CharImm(node.charValue))

        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod
    private fun translateStringLiteral(node: StringLiteralAST, cfgState: CfgState) {
        val reg = cfgState.newVar(StringType)
        val inst = AssignValue(reg, StrImm(node.stringValue))

        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod
    private fun translateVariableIdentifier(node: VariableIdentifierAST, cfgState: CfgState) {
        TODO()
    }

    @TranslatorMethod
    private fun translateNullPairLiteralAST(node: NullPairLiteralAST, cfgState: CfgState) {
        TODO()
    }

    @TranslatorMethod
    private fun translateArrayLiteral(node: ArrayLiteralAST, cfgState: CfgState) {
        TODO()
    }

    @TranslatorMethod
    private fun translateArrayElem(node: ArrayElemAST, cfgState: CfgState) {
        TODO()
    }

    @TranslatorMethod
    private fun translateAssignToArrayElem(node: AssignToArrayElemAST, cfgState: CfgState) {
        TODO()
    }

    @TranslatorMethod
    private fun translateFstPairElem(node: FstPairElemAST, cfgState: CfgState) {
        TODO()
    }

    @TranslatorMethod
    private fun translateSndPairElem(node: SndPairElemAST, cfgState: CfgState) {
        TODO()
    }

    @TranslatorMethod
    private fun translateAssignToPairElem(node: AssignToPairElemAST, cfgState: CfgState) {
        TODO()
    }

    @TranslatorMethod
    private fun translateUnOp(unOpExpr: UnaryOpExprAST, cfgState: CfgState) {
        TODO()
    }

    @TranslatorMethod
    private fun translateBinOp(expr: BinaryOpExprAST, cfgState: CfgState) {
        TODO()
    }

    private fun initialiseState(node: FunctionDeclarationAST, cfgState: CfgState) {
        cfgState.irFunction = IRFunction(node)
        cfgState.currentBlock = BasicBlock(cfgState.irFunction)
        for (param in node.formals) {
            cfgState.newVar(param.type)
        }
    }
}
