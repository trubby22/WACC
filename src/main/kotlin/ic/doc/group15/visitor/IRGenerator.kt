package ic.doc.group15.visitor

import ic.doc.group15.ast.*
import ic.doc.group15.ssa.BasicBlock
import ic.doc.group15.ssa.IRFunction
import ic.doc.group15.ssa.CFGState
import ic.doc.group15.type.BasicType

class CFGGenerator(private val enableLogging: Boolean = true) {

    private fun buildCFGFromProgram(program: AST): List<IRFunction> {
        log("Building CFG for program")
        val (functions, statements) = program.statements.partition { node -> node is FunctionDeclarationAST }
        // Build SSA form for each function
        val ssaIRFunctions = mutableListOf<IRFunction>()
        // Note that each function state is independent of each other
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
    private fun buildCFGFromFunctionDeclaration(CFGState: CFGState, node: FunctionDeclarationAST) {
        // Build CFG for the function
        initialiseState(CFGState, node)
        node.statements.forEach { buildCFG(CFGState, it) }
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
     **/
    private fun buildCFGFromIfBlock(cfgState: CFGState, node: IfBlockAST) {
        val thenBlock = BasicBlock(cfgState.irFunction)
        val elseBlock = BasicBlock(cfgState.irFunction)
        val exitBlock = BasicBlock(cfgState.irFunction)

        // add successors to entry block
        cfgState.currentBlock.addSuccessors(thenBlock, elseBlock)

        // construct then block
        cfgState.currentBlock = thenBlock
        node.statements.forEach { buildCFG(cfgState, it) }
        cfgState.currentBlock.addSuccessors(exitBlock)

        // construct else block
        cfgState.currentBlock = elseBlock
        node.elseBlock.statements.forEach { buildCFG(cfgState, it) }
        cfgState.currentBlock.addSuccessors(exitBlock)

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
    private fun buildCFGFromWhileBlock(cfgState: CFGState, node: WhileBlockAST) {
        val condBlock = BasicBlock(cfgState.irFunction)
        val loopBlock = BasicBlock(cfgState.irFunction)
        val exitBlock = BasicBlock(cfgState.irFunction)

        // add successors to current block and cond block
        cfgState.currentBlock.addSuccessors(condBlock)

        // construct cond block
        cfgState.currentBlock = condBlock
        buildCFG(cfgState, node.condExpr)
        cfgState.currentBlock.addSuccessors(loopBlock, exitBlock)

        // construct loop block
        cfgState.currentBlock = loopBlock
        node.statements.forEach { stat -> buildCFG(cfgState, stat) }
        cfgState.currentBlock.addSuccessors(condBlock)

        // set current block as exit block
        cfgState.currentBlock = exitBlock
    }

    private fun buildCFGFromBeginEndBlock(CFGState: CFGState, node: BeginEndBlockAST) {
        TODO("do we need this?")
    }

    // TODO(manage exit, return, other control flow constructs)
    private fun buildCFG(CFGState: CFGState, node: ASTNode) {
        when (node) {
            is WhileBlockAST -> buildCFGFromWhileBlock(CFGState, node)
            is IfBlockAST -> buildCFGFromIfBlock(CFGState, node)
            else -> CFGState.currentBlock.addInstructions(node)
        }
    }

    private fun initialiseState(CFGState: CFGState, node: FunctionDeclarationAST) {
        CFGState.irFunction = IRFunction(node)
        CFGState.currentBlock = BasicBlock(CFGState.irFunction)
    }

    private fun log(str: String) {
        if (enableLogging) {
            println(str)
        }
    }
}