package ic.doc.group15.ssa.cfg

import ic.doc.group15.ast.*
import ic.doc.group15.ssa.BasicBlock
import ic.doc.group15.ssa.IRFunction
import ic.doc.group15.ssa.tac.*
import ic.doc.group15.translator.TranslatorMethod
import ic.doc.group15.type.BasicType
import ic.doc.group15.type.Type
import ic.doc.group15.type.Variable
import ic.doc.group15.visitor.Visitor

/**
 * Convert the AST representation of the program to three-address code and build a control flow
 * graph for each function.
 *
 * Assumption: All nodes (read: basic blocks) can have at most two successors
 */
class CFGGenerator(private val enableLogging: Boolean = true): Visitor<ASTNode>() {

    @TranslatorMethod(AST::class)
    private fun buildCFGFromProgram(program: AST): List<IRFunction> {
        log("Building CFG for program")
        val (functions, statements) = program.statements.partition { node -> node is FunctionDeclarationAST }
        // Build SSA form for each function
        val ssaIRFunctions = mutableListOf<IRFunction>()
        // Note that each function state is independent of each other
        // TODO introduce concurrency 
        for (funcNode in functions) {
            val funcState = CFGState()
            buildCFGFromFunctionDeclaration(funcState, funcNode as FunctionDeclarationAST)
            ssaIRFunctions.add(funcState.irFunction)
        }

        // Construct main function separately (due to WACC language design constraints)
        val mainState = CFGState()
        buildCFGFromMain(mainState, program, *statements.toTypedArray())
        ssaIRFunctions.add(mainState.irFunction)

        return ssaIRFunctions
    }

    private fun buildCFGFromMain(CFGState: CFGState, program: AST, vararg stat: StatementAST) {
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
        if (statements.isEmpty()
            || statements.last() !is ReturnStatementAST
            || statements.last() !is ExitStatementAST) {
            val returnNode = ReturnStatementAST(mainNode, mainNode.symbolTable, IntLiteralAST(0))
            statements.add(returnNode)
        }

        buildCFGFromFunctionDeclaration(CFGState, mainNode)
    }

    /**
     * Populate the state with relevant information and build CFG for the function.
     * 1) Build CFG for each function
     * 2) Construct dominator tree and compute set of dominance frontier for each basic block
     * 3) Insert phi nodes
     * 4) Rename variables to obtain SSA form
     **/
    @TranslatorMethod(FunctionDeclarationAST::class)
    private fun buildCFGFromFunctionDeclaration(cfgState: CFGState, node: FunctionDeclarationAST) {
        log("Building CFG for function ${node.funcName}")
        // Build CFG for the function
        initialiseState(cfgState, node)
        node.statements.forEach { translate(cfgState, it) }

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
    @TranslatorMethod(IfBlockAST::class)
    private fun buildCFGFromIfBlock(cfgState: CFGState, node: IfBlockAST) {
        log("Building CFG for if block")
        val thenBlock = BasicBlock(cfgState.irFunction)
        val elseBlock = BasicBlock(cfgState.irFunction)
        val exitBlock = BasicBlock(cfgState.irFunction)

        // add successors to entry block
        cfgState.currentBlock.addSuccessors(thenBlock, elseBlock)

        // translate cond expr and store result in register
        translate(cfgState, node.condExpr)
        val resultReg = getResultRegister(cfgState)

        // add branch instruction to entry block
        val branchIfInst = BranchIf(resultReg, Label(thenBlock.toString()))
        cfgState.currentBlock.addInstructions(branchIfInst)

        // construct then block
        log("Building CFG for then block")
        cfgState.currentBlock = thenBlock
        node.statements.forEach { translate(cfgState, it) }
        if (!node.elseBlock.statements.any { it is ReturnStatementAST || it is ExitStatementAST }) {
            cfgState.currentBlock.addSuccessors(exitBlock)
        }

        // construct else block
        log("Building CFG for else block")
        cfgState.currentBlock = elseBlock
        node.elseBlock.statements.forEach { translate(cfgState, it) }
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
    @TranslatorMethod(WhileBlockAST::class)
    private fun buildCFGFromWhileBlock(cfgState: CFGState, node: WhileBlockAST) {
        log("Building CFG for while block")
        val condBlock = BasicBlock(cfgState.irFunction)
        val loopBlock = BasicBlock(cfgState.irFunction)
        val exitBlock = BasicBlock(cfgState.irFunction)

        // add successors to current block and cond block
        cfgState.currentBlock.addSuccessors(condBlock)

        // construct cond block
        cfgState.currentBlock = condBlock

        // translate cond expr and store result in register
        translate(cfgState, node.condExpr)
        val resultReg = getResultRegister(cfgState)

        // Branch to exit block if boolean condition is false
        val branchIfInst = BranchIf(resultReg, Label(exitBlock.toString()))
        cfgState.currentBlock.addInstructions(branchIfInst)

        // Add successors to current block for CFG analysis
        cfgState.currentBlock.addSuccessors(loopBlock, exitBlock)

        log("Building CFG for while loop block")
        // construct loop block
        cfgState.currentBlock = loopBlock
        node.statements.forEach { stat -> translate(cfgState, stat) }
        if (!node.statements.any { it is ReturnStatementAST || it is ExitStatementAST }) {
            cfgState.currentBlock.addSuccessors(condBlock)
        }

        // Add unconditional branch statement to condition block
        val branchInst = Branch(Label(condBlock.toString()))
        cfgState.currentBlock.addInstructions(branchInst)

        // set current block as exit block
        cfgState.currentBlock = exitBlock
    }

    @TranslatorMethod(BeginEndBlockAST::class)
    private fun buildCFGFromBeginEndBlock(cfgState: CFGState, node: BeginEndBlockAST) {
        val inScopeBlock = BasicBlock(cfgState.irFunction)
        val outScopeBlock = BasicBlock(cfgState.irFunction)

        // construct in scope block
        log("Building CFG for begin end block")
        cfgState.currentBlock = inScopeBlock
        node.statements.forEach { stat -> translate(cfgState, stat) }
        if (!node.statements.any { it is ReturnStatementAST || it is ExitStatementAST }) {
            cfgState.currentBlock.addSuccessors(outScopeBlock)
        }

        // set current block as exit block
        cfgState.currentBlock = outScopeBlock
    }

    @TranslatorMethod(CallAST::class)
    private fun translateFunctionCall(cfgState: CFGState, node: CallAST) {
        val paramRegs = mutableListOf<Operand>()
        for (arg in node.actuals) {
            translate(cfgState, arg)
            // Store the result for each param to a list
           paramRegs.add(getResultRegister(cfgState))
        }

        val type = node.funcIdent.returnType
        val f = CustomFunc(node.funcName, type)
        val reg = createTempVar(cfgState, type)
        val inst = AssignCall(reg, f, paramRegs)
        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod(VariableDeclarationAST::class)
    private fun translateVariableDeclaration(node: VariableDeclarationAST) {
        TODO()
    }

    @TranslatorMethod(AssignToIdentAST::class)
    private fun translateAssignToVariable(cfgState: CFGState, node: AssignToIdentAST) {
        // Store value in result register
        translate(cfgState, node.rhs)
        // Store reference of result register in variable map
        val resultReg = getResultRegister(cfgState)
        cfgState.varDefinedAt[node.lhs.ident] = resultReg
    }

    @TranslatorMethod(FreeStatementAST::class)
    private fun translateFreeStat(node: FreeStatementAST) {
        TODO()
    }

    @TranslatorMethod(ReturnStatementAST::class)
    private fun translateReturnStat(cfgState: CFGState, node: ReturnStatementAST) {
        // Translate the expression value and store it in a register
        translate(cfgState, node.expr)
        // Return value stored in latest register
        val resultReg = getResultRegister(cfgState)
        val inst = Call(Functions.RETURN, withParam(resultReg))
        cfgState.currentBlock.addInstructions(inst)

        // set successor to function exit point
        cfgState.currentBlock.addSuccessors(cfgState.irFunction.exitBlock)
    }

    // TODO(find a way to add an edge to end of program)
    @TranslatorMethod(ExitStatementAST::class)
    private fun translateExitStat(cfgState: CFGState, node: ExitStatementAST) {
        // Translate the expression value and store it in a register
        translate(cfgState, node.expr)
        // Exit and return exit code stored in result register
        val resultReg = getResultRegister(cfgState)
        val inst = Call(Functions.EXIT, withParam(resultReg))
        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod(PrintStatementAST::class)
    private fun translatePrintStat(cfgState: CFGState, node: PrintStatementAST) {
        translate(cfgState, node.expr)

        // Print value in register
        val reg = getResultRegister(cfgState)
        val f = Print(reg.type())
        val inst = Call(f, listOf(reg))
        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod(PrintlnStatementAST::class)
    private fun translatePrintlnStat(cfgState: CFGState, node: PrintlnStatementAST) {
        translate(cfgState, node.expr)

        // Print value in register
        val resultReg = getResultRegister(cfgState)
        val f = Println(resultReg.type())
        val inst = Call(f, withParam(resultReg))
        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod(ReadStatementAST::class)
    private fun translateReadStat(cfgState: CFGState, node: ReadStatementAST) {
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

    @TranslatorMethod(NewPairAST::class)
    private fun translateNewPair(node: NewPairAST) {
        TODO()
    }

    @TranslatorMethod(IntLiteralAST::class)
    private fun translateIntLiteral(cfgState: CFGState, node: IntLiteralAST) {
        val reg = createTempVar(cfgState, BasicType.IntType)
        val inst = AssignValue(reg, IntImm(node.intValue))

        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod(BoolLiteralAST::class)
    private fun translateBoolLiteral(cfgState: CFGState, node: BoolLiteralAST) {
        val reg = createTempVar(cfgState, BasicType.BoolType)
        val inst = AssignValue(reg, BoolImm(node.boolValue))

        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod(CharLiteralAST::class)
    private fun translateCharLiteral(cfgState: CFGState, node: CharLiteralAST) {
        val reg = createTempVar(cfgState, BasicType.CharType)
        val inst = AssignValue(reg, CharImm(node.charValue))

        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod(StringLiteralAST::class)
    private fun translateStringLiteral(cfgState: CFGState, node: StringLiteralAST) {
        val reg = createTempVar(cfgState, BasicType.StringType)
        val inst = AssignValue(reg, StrImm(node.stringValue))

        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod(VariableIdentifierAST::class)
    private fun translateVariableIdentifier(node: VariableIdentifierAST) {
        TODO()
    }

    @TranslatorMethod(NullPairLiteralAST::class)
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

    private fun createTempVar(cfgState: CFGState, type: Type): Var {
        val reg = Var(cfgState.smallestUnusedId++,"%t${cfgState.smallestUnusedId}",  type)
        cfgState.regList[reg.id] = reg
        return reg
    }

    private fun createVar(cfgState: CFGState, v: Variable, type: Type): Var {
        val reg = Var(cfgState.smallestUnusedId++, generateVarName(cfgState, v),  type)
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

    private fun initialiseState(cfgState: CFGState, node: FunctionDeclarationAST) {
        cfgState.irFunction = IRFunction(node)
        cfgState.currentBlock = BasicBlock(cfgState.irFunction)
        cfgState.smallestUnusedId += node.formals.size
    }

    private fun log(str: String) {
        if (enableLogging) {
            println(str)
        }
    }
}