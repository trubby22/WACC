package ic.doc.group15.ssa.cfg

import ic.doc.group15.ast.*
import ic.doc.group15.ssa.BasicBlock
import ic.doc.group15.ssa.ControlFlowGraph
import ic.doc.group15.ssa.IRFunction
import ic.doc.group15.ssa.tac.*
import ic.doc.group15.translator.TranslatorMethod
import ic.doc.group15.type.ArrayType
import ic.doc.group15.type.BasicType.*
import ic.doc.group15.type.PairType
import ic.doc.group15.type.ReturnableType
import ic.doc.group15.util.WORD
import ic.doc.group15.visitor.TranslatorVisitor

/**
 * Convert the AST representation of the program to three-address code and build a control flow
 * graph for each function.
 *
 * Assumption: All nodes (read: basic blocks) can have at most two successors
 */
class CfgGenerator(
    private val ast: AST,
    enableLogging: Boolean = true
) : TranslatorVisitor<ASTNode>() {

    private val cfg = ControlFlowGraph()

    /**
     * Generates the Control Flow Graph and returns it (should only be called once).
     */
    fun generate(): ControlFlowGraph {
        translate(ast)
        return cfg
    }

    @TranslatorMethod
    private fun buildCFGFromProgram(program: AST) {
        log("Building CFG for program")

        // REWORK THIS LINE WHEN MERGING INTO MASTER
        val (functions, statements) = program.statements.partition { it is FunctionDeclarationAST }

        // Note that each function state is independent of each other
        // TODO introduce concurrency
        for (funcNode in functions) {
            val funcState = CfgState()
            buildCFGFromFunctionDeclaration(funcNode as FunctionDeclarationAST, funcState)
            cfg.addFunction(funcState.irFunction)
        }

        // Construct main function separately (due to WACC language design constraints)
        val mainState = CfgState()
        buildCFGFromMain(mainState, program, *statements.toTypedArray())
        cfg.addFunction(mainState.irFunction)
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

        // Complete variable set
        cfgState.irFunction.variableSet = cfgState.varSet()
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
        val branchIfInst = TacBranchIf(resultReg, thenBlock)
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
        val branchInst = TacBranch(exitBlock)
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
        val branchIfInst = TacBranchIf(resultReg, exitBlock)
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
        val branchInst = TacBranch(condBlock)
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
        val paramRegs = mutableListOf<TacOperand>()
        for (arg in node.actuals) {
            translate(arg, cfgState)
            // Store the result for each param to a list
           paramRegs.add(cfgState.resultRegister)
        }

        val type = node.funcIdent.returnType
        val f = CustomFunc(node.funcName, type)
        val reg = cfgState.newVar(type)
        val inst = TacAssignCall(reg, f, *paramRegs.toTypedArray())
        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod
    private fun translateVariableDeclaration(node: VariableDeclarationAST, cfgState: CfgState) {
        translate(node.rhs)
        val resultReg = cfgState.resultRegister

        // Add reference to the variable map
        val v = node.varIdent
        cfgState.varDefinedAt[v] = resultReg
    }

    @TranslatorMethod
    private fun translateAssignToVariable(node: AssignToIdentAST, cfgState: CfgState) {
        // Store value in result register
        translate(node.rhs, cfgState)
        val resultReg = cfgState.resultRegister

        // Store reference of result register in variable map
        val v = node.lhs.ident
        cfgState.varDefinedAt[v] = resultReg
    }

    @TranslatorMethod
    private fun translateFreeStat(node: FreeStatementAST, cfgState: CfgState) {
        val v = (node.expr as VariableIdentifierAST).ident
        val addrReg = cfgState.varDefinedAt[v]!!

        val inst = TacCall(Functions.FREE, addrReg)
        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod
    private fun translateReturnStat(node: ReturnStatementAST, cfgState: CfgState) {
        // Translate the expression value and store it in a register
        translate(node.expr, cfgState)
        // Return value stored in latest register
        val resultReg = cfgState.resultRegister
        val inst = TacCall(Functions.RETURN, resultReg)
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
        val inst = TacCall(Functions.EXIT, resultReg)
        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod
    private fun translatePrintStat(node: PrintStatementAST, cfgState: CfgState) {
        translate(node.expr, cfgState)

        // Print value in register
        val reg = cfgState.resultRegister
        val inst = TacCall(Functions.PRINT, reg)
        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod
    private fun translatePrintlnStat(node: PrintlnStatementAST, cfgState: CfgState) {
        translate(node.expr, cfgState)

        // Print value in register
        val resultReg = cfgState.resultRegister
        val inst = TacCall(Functions.PRINTLN, resultReg)
        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod
    private fun translateReadStat(node: ReadStatementAST, cfgState: CfgState) {
        // Find if there is a previous reference
        when (val lhs = node.target.lhs) {
            is VariableIdentifierAST -> translate(lhs, cfgState)
            is ArrayElemAST -> translate(lhs, cfgState)
            is PairElemAST -> translate(lhs, cfgState)
            else -> throw IllegalArgumentException("cannot read into expression $lhs")
        }

        // Get variable referring to LHS
        val reg = cfgState.resultRegister

        // Perform read instruction
        val inst = TacCall(Functions.READ, reg)

        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod
    private fun translateNewPair(node: NewPairAST, cfgState: CfgState) {
        // Allocate memory for two addresses in pair construction
        val addrReg = cfgState.newVar(IntType)
        val mallocInst = TacAllocate(addrReg, IntImm(2 * WORD))
        cfgState.currentBlock.addInstructions(mallocInst)

        listOf(node.fstExpr, node.sndExpr).forEachIndexed { index, expr ->
            // Translate each expression and store it in a variable
            translate(expr, cfgState)
            val resultReg = cfgState.resultRegister
            val exprType = (resultReg.type) as ReturnableType

            // Allocate memory for each sub-expression and store expression value into memory
            val subAddrReg = cfgState.newVar(exprType)
            val subMallocInst = TacAllocate(subAddrReg, IntImm(exprType.size()))
            val storeValueInst = TacStore(subAddrReg, resultReg)

            // Store address malloced for sub-expression into the base newpair address
            val baseAddrReg = cfgState.newVar(IntType)
            val offsetInst = TacAssignBinOp(baseAddrReg, BinaryOp.PLUS, addrReg, IntImm(index * WORD))
            val storeAddrInst = TacStore(baseAddrReg, subAddrReg)

            cfgState.currentBlock.addInstructions(
                subMallocInst,
                storeValueInst,
                offsetInst,
                storeAddrInst
            )
        }
    }

    @TranslatorMethod
    private fun translateIntLiteral(node: IntLiteralAST, cfgState: CfgState) {
        val reg = cfgState.newVar(IntType)
        val inst = TacAssignValue(reg, IntImm(node.intValue))

        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod
    private fun translateBoolLiteral(node: BoolLiteralAST, cfgState: CfgState) {
        val reg = cfgState.newVar(BoolType)
        val inst = TacAssignValue(reg, BoolImm(node.boolValue))

        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod
    private fun translateCharLiteral(node: CharLiteralAST, cfgState: CfgState) {
        val reg = cfgState.newVar(CharType)
        val inst = TacAssignValue(reg, CharImm(node.charValue))

        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod
    private fun translateStringLiteral(node: StringLiteralAST, cfgState: CfgState) {
        val reg = cfgState.newVar(StringType)
        val inst = TacAssignValue(reg, StrImm(node.stringValue))

        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod
    private fun translateVariableIdentifier(node: VariableIdentifierAST, cfgState: CfgState) {
        val v = node.ident
        val varReg = cfgState.varDefinedAt[v]!!

        val reg = cfgState.newVar(varReg.type)
        val moveInst = TacAssignValue(reg, varReg)

        cfgState.currentBlock.addInstructions(moveInst)
    }

    @TranslatorMethod
    private fun translateNullPairLiteralAST(node: NullPairLiteralAST, cfgState: CfgState) {
        val reg = cfgState.newVar(IntType)
        val inst = TacAssignValue(reg, IntImm(0))

        cfgState.currentBlock.addInstructions(inst)
    }

    // Note: Only 1D array can be created at a time. To create multidimensional arrays, we
    // create another array which holds a list of addresses/references to sub-dimensional arrays
    @TranslatorMethod
    private fun translateArrayLiteral(node: ArrayLiteralAST, cfgState: CfgState) {
        // Get element type and infer size
        val elems = node.elems
        val elemSize = if (elems.isEmpty()) 0 else elems.first().type.size()
        val arrSize = elems.size

        // Calculate the memory size to be allocated
        val mallocSize = WORD + arrSize * elemSize

        // Allocate memory
        assert(node.type is ArrayType)
        val addrReg = cfgState.newVar(node.type)
        val mallocInst = TacAllocate(addrReg, IntImm(mallocSize))

        // First position used to store array length
        val lenReg = cfgState.newVar(IntType)
        val passValInst = TacAssignValue(lenReg, IntImm(arrSize))
        val storeLenInst = TacStore(lenReg, addrReg)

        // Add instructions
        cfgState.currentBlock.addInstructions(mallocInst, passValInst, storeLenInst)

        // Store subsequent values into the array
        var offset = WORD
        for (expr in elems) {
            // Translate expression
            translate(expr, cfgState)
            val resultReg = cfgState.resultRegister

            // Store value into array at corresponding position
            val offsetAddrReg = cfgState.newVar(IntType)
            val offsetAddrInst = TacAssignBinOp(offsetAddrReg, BinaryOp.PLUS, addrReg, IntImm(offset))
            val storeInst = TacStore(resultReg, offsetAddrReg)

            // Add instructions
            cfgState.currentBlock.addInstructions(offsetAddrInst, storeInst)

            // Increment offset
            offset += elemSize
        }
    }

    @TranslatorMethod
    private fun translateArrayElem(node: ArrayElemAST, cfgState: CfgState) {
        // Compute address containing element in the array
        translateAddress(node, cfgState)
        val addrReg = cfgState.resultRegister

        // Load value from address
        val valueReg = cfgState.newVar(node.elemType)
        val loadInst = TacLoad(valueReg, addrReg)

        // Add instruction
        cfgState.currentBlock.addInstructions(loadInst)
    }

    @TranslatorMethod
    private fun translateAssignToArrayElem(node: AssignToArrayElemAST, cfgState: CfgState) {
        // Translate RHS expression
        translate(node.rhs, cfgState)
        val resultReg = cfgState.resultRegister

        // Calculate offset
        TODO()
    }

    @TranslatorMethod
    private fun translateFstPairElem(node: FstPairElemAST, cfgState: CfgState) {
        translateAddress(node, cfgState)
        val addrReg = cfgState.resultRegister

        // Load the value in memory into variable
        val reg = cfgState.newVar(node.elemType)
        val inst = TacLoad(reg, addrReg)

        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod
    private fun translateSndPairElem(node: SndPairElemAST, cfgState: CfgState) {
        translateAddress(node, cfgState)
        val addrReg = cfgState.resultRegister

        // Load the value in memory into variable
        val reg = cfgState.newVar(node.elemType)
        val inst = TacLoad(reg, addrReg)

        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod
    private fun translateAssignToPairElem(node: AssignToPairElemAST, cfgState: CfgState) {
        // Translate expression
        translate(node.rhs, cfgState)
        val resultReg = cfgState.resultRegister

        // Get base address of pair variable in memory
        translateAddress(node.lhs, cfgState)
        val addrReg = cfgState.resultRegister

        // Add store instruction
        val storeInst = TacStore(resultReg, addrReg)
        cfgState.currentBlock.addInstructions(storeInst)
    }

    @TranslatorMethod
    private fun translateUnOp(unOpExpr: UnaryOpExprAST, cfgState: CfgState) {
        // Translate expression
        translate(unOpExpr.expr, cfgState)
        val resultReg = cfgState.resultRegister
        // Create a new variable to store the result of binary operation
        val reg = cfgState.newVar(unOpExpr.expr.type)

        val inst = when (unOpExpr.operator) {
            UnaryOp.BANG -> TacAssignCall(reg, Functions.BANG, resultReg)
            UnaryOp.MINUS -> TacAssignValue(reg, resultReg)
            UnaryOp.LEN -> TacAssignCall(reg, Functions.LEN, resultReg)
            UnaryOp.ORD -> TacAssignCall(reg, Functions.ORD, resultReg)
            UnaryOp.CHR -> TacAssignCall(reg, Functions.CHR, resultReg)
        }

        cfgState.currentBlock.addInstructions(inst)
    }

    @TranslatorMethod
    private fun translateBinOp(expr: BinaryOpExprAST, cfgState: CfgState) {
        // Translate LHS expression
        translate(expr.expr1, cfgState)
        val x = cfgState.resultRegister
        // Translate RHS expression
        translate(expr.expr2, cfgState)
        val y = cfgState.resultRegister

        val op = expr.operator
        // Create a new variable to store the result of binary operation
        val reg = cfgState.newVar(op.returnType)

        val inst = TacAssignBinOp(reg, op, x, y)
        cfgState.currentBlock.addInstructions(inst)
    }

    private fun translateAddress(node: PairElemAST, cfgState: CfgState) {
        // Get base address of pair variable
        translate(node.expr, cfgState)
        val addrReg = cfgState.resultRegister
        // For backend analysis to insert null check before loading the address
        assert(addrReg.type() is PairType)

        if (node is SndPairElemAST) {
            // Find the base address of second element of pair
            val secondElemAddrReg = cfgState.newVar(addrReg.type())
            val addrInst = TacAssignBinOp(secondElemAddrReg, BinaryOp.PLUS, secondElemAddrReg, IntImm(WORD))

            cfgState.currentBlock.addInstructions(addrInst)
        }
    }

    private fun translateAddress(node: ArrayElemAST, cfgState: CfgState) {
        // Get reference to the reg storing the address
        val v = node.arrayVar.ident
        val addrReg = cfgState.varDefinedAt[v]!!

        // Compute the address of element in the (potentially nested) array
        for (expr in node.indexExpr) {
            // Translate index expression
            translate(expr, cfgState)
            val currAddrReg = cfgState.resultRegister

            val loadInst = TODO()
        }
    }

    private fun initialiseState(node: FunctionDeclarationAST, cfgState: CfgState) {
        cfgState.irFunction = IRFunction(node)
        cfgState.currentBlock = BasicBlock(cfgState.irFunction)
        for (param in node.formals) {
            cfgState.newVar(param.type)
            val argumentInst = Argument(cfgState.resultRegister)
            cfgState.currentBlock.addInstructions(argumentInst)
        }
    }
}
