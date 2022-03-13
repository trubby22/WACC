package ic.doc.group15.visitor

import ic.doc.group15.ast.*
import ic.doc.group15.ssa.BasicBlock
import ic.doc.group15.ssa.IRFunction
import ic.doc.group15.ssa.CFGState
import ic.doc.group15.type.BasicType

/**
 * Assumption: All nodes (read: basic blocks) can have at most two successors
 */
class CFGGenerator(private val enableLogging: Boolean = true) {

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
    private fun buildCFGFromFunctionDeclaration(cfgState: CFGState, node: FunctionDeclarationAST) {
        log("Building CFG for function ${node.funcName}")
        // Build CFG for the function
        initialiseState(cfgState, node)
        node.statements.forEach { buildCFG(cfgState, it) }

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
     **/
    private fun buildCFGFromIfBlock(cfgState: CFGState, node: IfBlockAST) {
        log("Building CFG for if block")
        val thenBlock = BasicBlock(cfgState.irFunction)
        val elseBlock = BasicBlock(cfgState.irFunction)
        val exitBlock = BasicBlock(cfgState.irFunction)

        // add successors to entry block
        cfgState.currentBlock.addSuccessors(thenBlock, elseBlock)

        // add branch instruction to entry block
        val branchIfInst = BranchIfAST(
            node.parent,
            node.symbolTable,
            node.condExpr,
            thenBlock,
            elseBlock
        )
        cfgState.currentBlock.addInstructions(branchIfInst)

        // construct then block
        log("Building CFG for then block")
        cfgState.currentBlock = thenBlock
        node.statements.forEach { buildCFG(cfgState, it) }
        if (!node.elseBlock.statements.any { it is ReturnStatementAST || it is ExitStatementAST }) {
            cfgState.currentBlock.addSuccessors(exitBlock)
        }

        // construct else block
        log("Building CFG for else block")
        cfgState.currentBlock = elseBlock
        node.elseBlock.statements.forEach { buildCFG(cfgState, it) }
        if (!node.elseBlock.statements.any { it is ReturnStatementAST || it is ExitStatementAST }) {
            cfgState.currentBlock.addSuccessors(exitBlock)
        }

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
        log("Building CFG for while block")
        val condBlock = BasicBlock(cfgState.irFunction)
        val loopBlock = BasicBlock(cfgState.irFunction)
        val exitBlock = BasicBlock(cfgState.irFunction)

        // add successors to current block and cond block
        cfgState.currentBlock.addSuccessors(condBlock)

        // construct cond block
        cfgState.currentBlock = condBlock
        val branchIfInst = BranchIfAST(
            node.parent,
            node.symbolTable,
            node.condExpr,
            loopBlock,
            exitBlock
        )
        cfgState.currentBlock.addInstructions(branchIfInst)
        cfgState.currentBlock.addSuccessors(loopBlock, exitBlock)

        log("Building CFG for while loop block")
        // construct loop block
        cfgState.currentBlock = loopBlock
        node.statements.forEach { stat -> buildCFG(cfgState, stat) }
        if (!node.statements.any { it is ReturnStatementAST || it is ExitStatementAST }) {
            cfgState.currentBlock.addSuccessors(condBlock)
        }

        // set current block as exit block
        cfgState.currentBlock = exitBlock
    }

    private fun buildCFGFromBeginEndBlock(cfgState: CFGState, node: BeginEndBlockAST) {
        val inScopeBlock = BasicBlock(cfgState.irFunction)
        val outScopeBlock = BasicBlock(cfgState.irFunction)

        // construct in scope block
        log("Building CFG for begin end block")
        cfgState.currentBlock = inScopeBlock
        node.statements.forEach { stat -> buildCFG(cfgState, stat) }
        if (!node.statements.any { it is ReturnStatementAST || it is ExitStatementAST }) {
            cfgState.currentBlock.addSuccessors(outScopeBlock)
        }

        // set current block as exit block
        cfgState.currentBlock = outScopeBlock
    }

    private fun buildCFGFromReturnStatement(cfgState: CFGState, node: ReturnStatementAST) {
        log("Building CFG for block that ends with return statement")
        cfgState.currentBlock.addInstructions(node)
        // set successor to function exit point
        cfgState.currentBlock.addSuccessors(cfgState.irFunction.exitBlock)
    }

    // TODO(manage exit, call subroutine, other control flow constructs)
    private fun buildCFG(cfgState: CFGState, node: ASTNode) {
        when (node) {
            is WhileBlockAST -> buildCFGFromWhileBlock(cfgState, node)
            is IfBlockAST -> buildCFGFromIfBlock(cfgState, node)
            is BeginEndBlockAST -> buildCFGFromBeginEndBlock(cfgState, node)
            is ReturnStatementAST -> buildCFGFromReturnStatement(cfgState, node)
            else -> {
                log("Adding instruction of AST type ${node::class.simpleName}")
                cfgState.currentBlock.addInstructions(node)
            }
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