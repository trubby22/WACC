package ic.doc.group15.ssa.cfg

import ic.doc.group15.ast.*
import ic.doc.group15.ssa.BasicBlock
import ic.doc.group15.ssa.IRFunction
import ic.doc.group15.ssa.tac.*
import ic.doc.group15.translator.AssemblyGenerator
import ic.doc.group15.translator.TranslatorMethod
import ic.doc.group15.type.BasicType
import ic.doc.group15.type.Type
import ic.doc.group15.type.Variable

/**
 * Convert the AST representation of the program to three-address code and build a control flow
 * graph for each function.
 *
 * Assumption: All nodes (read: basic blocks) can have at most two successors
 */
class CFGGenerator(
    ast: AST,
    enableLogging: Boolean = true
) : AssemblyGenerator<ASTNode>(ast, enableLogging) {

    @TranslatorMethod
    private fun buildCFGFromProgram(program: AST): List<IRFunction> {
        log("Building CFG for program")
        val (functions, statements) = program.statements.partition { node -> node is FunctionDeclarationAST }
        // Build SSA form for each function
        val ssaIRFunctions = mutableListOf<IRFunction>()
        // Note that each function state is independent of each other
        // TODO introduce concurrency
        for (funcNode in functions) {
            val funcState = CFGState()
            buildCFGFromFunctionDeclaration(funcNode as FunctionDeclarationAST, funcState)
            ssaIRFunctions.add(funcState.irFunction)
        }

        // Construct main function separately (due to WACC language design constraints)
        val mainState = CFGState()
        buildCFGFromMain(mainState, program, *statements.toTypedArray())
        ssaIRFunctions.add(mainState.irFunction)

        return ssaIRFunctions
    }

    private fun buildCFGFromMain(cfgState: CFGState, program: AST, vararg stat: StatementAST) {
        val mainNode = FunctionDeclarationAST(
            program,
            program.symbolTable,
            program.symbolTable.subScope(),
            BasicType.IntType,
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
    private fun buildCFGFromFunctionDeclaration(node: FunctionDeclarationAST, cfgState: CFGState) {
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
    private fun buildCFGFromIfBlock(node: IfBlockAST, cfgState: CFGState) {
        log("Building CFG for if block")
        val thenBlock = BasicBlock(cfgState.irFunction)
        val elseBlock = BasicBlock(cfgState.irFunction)
        val exitBlock = BasicBlock(cfgState.irFunction)

        // add successors to entry block
        cfgState.currentBlock.addSuccessors(thenBlock, elseBlock)

        // translate cond expr and store result in register
        translate(node.condExpr, cfgState)
        val resultReg = getResultRegister(cfgState)

        // add branch instruction to entry block
        val branchIfInst = BranchIf(resultReg, Label(thenBlock.toString()))
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
        val branchInst = Branch(Label(exitBlock.toString()))
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
    private fun buildCFGFromWhileBlock(node: WhileBlockAST, cfgState: CFGState) {
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
        val resultReg = getResultRegister(cfgState)

        // Branch to exit block if boolean condition is false
        val branchIfInst = BranchIf(resultReg, Label(exitBlock.toString()))
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
        val branchInst = Branch(Label(condBlock.toString()))
        cfgState.currentBlock.addInstructions(branchInst)

        // set current block as exit block
        cfgState.currentBlock = exitBlock
    }

    @TranslatorMethod
    private fun buildCFGFromBeginEndBlock(node: BeginEndBlockAST, cfgState: CFGState) {
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
    private fun translateFunctionCall(node: CallAST, cfgState: CFGState) {
        val paramRegs = mutableListOf<Operand>()
        for (arg in node.actuals) {
            translate(arg, cfgState)
            // Store the result for each param to a list
           paramRegs.add(getResultRegister(cfgState))
        }

        val type = node.funcIdent.returnType
        val f = CustomFunc(node.funcName, type)
        val reg = createTempVar(cfgState, type)
        val inst = AssignCall(reg, f, paramRegs)
        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod
    private fun translateVariableDeclaration(node: VariableDeclarationAST, cfgState: CFGState) {
        TODO()
    }

    @TranslatorMethod
    private fun translateAssignToVariable(node: AssignToIdentAST, cfgState: CFGState) {
        // Store value in result register
        translate(node.rhs, cfgState)
        // Store reference of result register in variable map
        val resultReg = getResultRegister(cfgState)
        cfgState.varDefinedAt[node.lhs.ident] = resultReg
    }

    @TranslatorMethod
    private fun translateFreeStat(node: FreeStatementAST, cfgState: CFGState) {
        TODO()
    }

    @TranslatorMethod
    private fun translateReturnStat(node: ReturnStatementAST, cfgState: CFGState) {
        // Translate the expression value and store it in a register
        translate(node.expr, cfgState)
        // Return value stored in latest register
        val resultReg = getResultRegister(cfgState)
        val inst = Call(Functions.RETURN, withParam(resultReg))
        cfgState.currentBlock.addInstructions(inst)

        // set successor to function exit point
        cfgState.currentBlock.addSuccessors(cfgState.irFunction.exitBlock)
    }

    // TODO(find a way to add an edge to end of program)
    @TranslatorMethod
    private fun translateExitStat(node: ExitStatementAST, cfgState: CFGState) {
        // Translate the expression value and store it in a register
        translate(node.expr, cfgState)
        // Exit and return exit code stored in result register
        val resultReg = getResultRegister(cfgState)
        val inst = Call(Functions.EXIT, withParam(resultReg))
        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod
    private fun translatePrintStat(node: PrintStatementAST, cfgState: CFGState) {
        translate(node.expr, cfgState)

        // Print value in register
        val reg = getResultRegister(cfgState)
        val f = Print(reg.type())
        val inst = Call(f, listOf(reg))
        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod
    private fun translatePrintlnStat(node: PrintlnStatementAST, cfgState: CFGState) {
        translate(node.expr, cfgState)

        // Print value in register
        val resultReg = getResultRegister(cfgState)
        val f = Println(resultReg.type())
        val inst = Call(f, withParam(resultReg))
        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod
    private fun translateReadStat(node: ReadStatementAST, cfgState: CFGState) {
        // Find if there is a previous reference
        val param = when (val lhs = node.target.lhs) {
            is VariableIdentifierAST -> cfgState.varDefinedAt[lhs.ident]
            is ArrayElemAST -> TODO()
            is PairElemAST -> TODO()
            else -> throw IllegalArgumentException("cannot read into expression $lhs")
        }

        // Store read value in new register
        val type = node.target.type
        val f = Read(type)
        val reg = createTempVar(cfgState, type)
        val inst = AssignCall(reg, f, withParam(param!!))

        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod
    private fun translateNewPair(node: NewPairAST, cfgState: CFGState) {
        TODO()
    }

    @TranslatorMethod
    private fun translateIntLiteral(node: IntLiteralAST, cfgState: CFGState) {
        val reg = createTempVar(cfgState, BasicType.IntType)
        val inst = AssignValue(reg, IntImm(node.intValue))

        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod
    private fun translateBoolLiteral(node: BoolLiteralAST, cfgState: CFGState) {
        val reg = createTempVar(cfgState, BasicType.BoolType)
        val inst = AssignValue(reg, BoolImm(node.boolValue))

        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod
    private fun translateCharLiteral(node: CharLiteralAST, cfgState: CFGState) {
        val reg = createTempVar(cfgState, BasicType.CharType)
        val inst = AssignValue(reg, CharImm(node.charValue))

        cfgState.currentBlock.addInstructions(inst)
    }

        @TranslatorMethod
        private fun translateStringLiteral(node: StringLiteralAST, cfgState: CFGState) {
        val reg = createTempVar(cfgState, BasicType.StringType)
        val inst = AssignValue(reg, StrImm(node.stringValue))

        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod
    private fun translateVariableIdentifier(node: VariableIdentifierAST, cfgState: CFGState) {
        TODO()
    }

    @TranslatorMethod
    private fun translateNullPairLiteralAST(node: NullPairLiteralAST, cfgState: CFGState) {
        TODO()
    }

    @TranslatorMethod
    private fun translateArrayLiteral(node: ArrayLiteralAST, cfgState: CFGState) {
        TODO()
    }

    @TranslatorMethod
    private fun translateArrayElem(node: ArrayElemAST, cfgState: CFGState) {
        TODO()
    }

    @TranslatorMethod
    private fun translateAssignToArrayElem(node: AssignToArrayElemAST, cfgState: CFGState) {
        TODO()
    }

    @TranslatorMethod
    private fun translateFstPairElem(node: FstPairElemAST, cfgState: CFGState) {
        TODO()
    }

    @TranslatorMethod
    private fun translateSndPairElem(node: SndPairElemAST, cfgState: CFGState) {
        TODO()
    }

    @TranslatorMethod
    private fun translateAssignToPairElem(node: AssignToPairElemAST, cfgState: CFGState) {
        TODO()
    }

    @TranslatorMethod
    private fun translateUnOp(unOpExpr: UnaryOpExprAST, cfgState: CFGState) {
        TODO()
    }

    @TranslatorMethod
    private fun translateBinOp(expr: BinaryOpExprAST, cfgState: CFGState) {
        TODO()
    }

    private fun createTempVar(cfgState: CFGState, type: Type): Var {
        val reg = Var(cfgState.smallestUnusedId++, "%t${cfgState.smallestUnusedId}", type)
        cfgState.regList[reg.id] = reg
        return reg
    }

    private fun createVar(cfgState: CFGState, v: Variable, type: Type): Var {
        val reg = Var(cfgState.smallestUnusedId++, generateVarName(cfgState, v), type)
        cfgState.regList[reg.id] = reg
        return reg
    }

    private fun generateVarName(cfgState: CFGState, v: Variable): String {
        TODO()
    }

    private fun getResultRegister(cfgState: CFGState): Var {
        return cfgState.regList[cfgState.smallestUnusedId - 1]!!
    }

    private fun withParam(vararg arguments: Operand): List<Operand> {
        return arguments.toList()
    }

    private fun initialiseState(node: FunctionDeclarationAST, cfgState: CFGState) {
        cfgState.irFunction = IRFunction(node)
        cfgState.currentBlock = BasicBlock(cfgState.irFunction)
        cfgState.smallestUnusedId += node.formals.size
    }
}
