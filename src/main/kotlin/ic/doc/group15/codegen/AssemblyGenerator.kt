package ic.doc.group15.codegen

import ic.doc.group15.ast.*
import ic.doc.group15.codegen.assembly.*
import ic.doc.group15.codegen.assembly.UtilFunction.P_READ_INT
import ic.doc.group15.codegen.assembly.instruction.*
import ic.doc.group15.codegen.assembly.instruction.ConditionCode.*
import ic.doc.group15.codegen.assembly.operand.*
import ic.doc.group15.codegen.assembly.operand.Register.*
import ic.doc.group15.type.BasicType.*
import ic.doc.group15.type.Identifier
import ic.doc.group15.type.Variable
import java.util.*

const val START_VAL = 0

class AssemblyGenerator {

    val state : State = State()

    var sp: Int = START_VAL - 1

    /**
     * Represents the ".data" section of the assembly code.
     *
     * Contains info for raw data in memory, such as string literals.
     */
    private val data: MutableMap<String, Data> = mutableMapOf()
    private val utilData: MutableMap<String, Data> = mutableMapOf()

    /**
     * Represents the ".text" section of the assembly code.
     *
     * Contains labels that can be branched to, and the main function.
     */
    private val text: MutableMap<String, BranchLabel> = mutableMapOf()
    private val utilText: MutableMap<String, BranchLabel> = mutableMapOf()

    private val stringLabel = UniqueStringLabel()
    private val branchLabel = UniqueBranchLabel()

    // when we enter a block, we will prematurely calculate how much stack space that block will need by summing the
    // size in bytes of each of the variables in its symbol table. then we will be able to decrement the stack pointer
    // by this amount in one go leaving enough space for the execution of the entire block. the below function takes
    // in a block and returns the amount of stack space it will require
    fun requiredStackSpace(node: BlockAST): Int {
        var stackSpace = 0
        val st = node.symbolTable
        val map = st.getMap()
        val list: List<Identifier> = map.values as List<Identifier>
        for (i in list) {
            if (i is Variable) {
                stackSpace += i.type.sizeInBytes()
            }
        }
        return stackSpace
    }

    fun transProgram(program: BlockAST) : List<Line> {
        val instructions = mutableListOf<Line>()
        val statements: List<StatementAST> = program.statements
        for (stat in statements) {
            instructions.addAll(transStat(stat, R4))
        }
        return instructions
    }

    // generates assembly code for a VariableDeclarationAST node and returns the list of instructions
    fun transBlock(block: BlockAST, resultReg: Register): List<Line> {
        val instructions = mutableListOf<Line>()
        var stackSpace = requiredStackSpace(block)
        sp -= stackSpace
        instructions.add(Sub(SP, SP, ImmediateOperand(stackSpace)))
        val statements: List<StatementAST> = block.statements
        for (stat in statements) {
            instructions.addAll(transStat(stat, resultReg))
        }
        sp += stackSpace
        instructions.add(Add(SP, SP, ImmediateOperand(stackSpace)))
//        this.state.popStacks(block.symbolTable.getMap().keys.parallelStream().toList()) // read comments in the State class to understand why we do this
        return instructions
    }

    //region statement

    fun transFunctionDeclaration(funcDec : FunctionDeclarationAST) : List<Line> {

        // i think im gonna have to push all registers to stack then pop
        // before and after a function call cus if you look at the reference compiler,
        // the function definitions always assume they can use whatever registers
        // they want, so i assume the registers will have been pushed to the stack.

        // however... it seems that there is no situation where a function
        // can be called while there are registers in use actually so maybe
        // i dont need to do that

        val instructions = mutableListOf<Line>()
        var pos = 0
        for (i in funcDec.formals) {
            state.setStackPos(i.varName, sp + pos)
            pos += i.type.sizeInBytes()
        }
        val stackSpace = requiredStackSpace(funcDec) - pos // the required stack space function will take into account the parameters as they are part of the symbol table for functions but we have already taken these into account
        sp-= stackSpace
        // functions are missing labels for now as well as .ltorg at the end
        instructions.addAll(mutableListOf(
            Push(Register.LR),
            Sub(SP, SP, ImmediateOperand(stackSpace))
        ))
//        instructions.addAll(transBlock((funcDec) as BlockAST, 4))
        instructions.add(Move(R0, R4))
        instructions.add(Add(SP, SP, ImmediateOperand(stackSpace)))
        instructions.add(Pop(PC))
        instructions.add(Pop(PC))
        return instructions
    }

    fun transCall(call : CallAST, resultReg: Register) {
        val instructions = mutableListOf<Line>()
        // parameters will be put onto the stack in reverse order e.g. f(a, b, c)
        // stack will look like so:
        // c
        // b
        // a
        val revParams = call.actuals.reverse() as MutableList<ExpressionAST>
        // below we decrement the stack pointer by the amount needed to
        // store the type of that parameter then put that parameter on
        // the stack at that position. we also keep track of spDec (the
        // total amount we had to decrement the stack pointer for the
        // arguments) so that after executing the function we can add
        // the same amount back to the stack pointer
        var spDec = 0
        for (i in revParams) {
            sp -= i.type.sizeInBytes()
            spDec += i.type.sizeInBytes()
            instructions.addAll(transExp(i, resultReg))
            instructions.add(Sub(SP, SP, ImmediateOperand(-i.type.sizeInBytes())))
            instructions.add(StoreWord(SP, ZeroOffset(resultReg)))
        }
        instructions.add(BranchLink("p_" + call.funcName))
        instructions.add(Add(SP, SP, ImmediateOperand(spDec)))
        instructions.add(Move(resultReg, R0))
    }

    // generates the assembly code for a BlockAST node and returns the list of instructions
    fun transStat(stat: StatementAST, resultReg: Register): List<Line> {
        val instructions = mutableListOf<Line>()
        when (stat) {
            is SkipStatementAST -> {}
            is VariableDeclarationAST -> instructions.addAll(transVariableDeclaration(stat, resultReg))
            is AssignToIdentAST -> instructions.addAll(transAssignToIdent(stat, resultReg))
            is ReadStatementAST -> instructions.addAll(transReadStatement(stat, resultReg))
            // is FreeStatementAST ->
            // is PrintStatementAST ->
            // is PrintlnStatementAST ->
            is IfBlockAST -> instructions.addAll(transIfBlock(stat, resultReg))
            // is WhileBlockAST ->
            // is BeginEndBlockAST ->
            is SequenceStatementAST -> instructions.addAll(transSequenceStatement(stat, resultReg))
        }
        return instructions
    }

    // generates assembly code for a VariableDeclarationAST node and returns the list of instructions
    fun transVariableDeclaration(node: VariableDeclarationAST, resultReg: Register): List<Line> {
        val instructions = mutableListOf<Line>()
        sp -= node.varIdent.type.sizeInBytes()
        state.setStackPos(node.varName, sp)
        instructions.addAll(transAssignRhs(node.rhs, resultReg))
        instructions.add(StoreWord(SP, ImmediateOffset(resultReg, state.getStackPos(node.varName))))
        return instructions
    }

    // this function will generate the assembly code for an AssignToIdentAST node and return the list of instructions
    fun transAssignToIdent(node: AssignToIdentAST, resultReg: Register): List<Line> {
        val instructions = mutableListOf<Line>()
        instructions.addAll(transAssignRhs(node.rhs, resultReg))
        instructions.add(StoreWord(SP, ImmediateOffset(resultReg, state.getStackPos((node.lhs as VariableIdentifierAST).varName) - sp))) // dk y but it assumed node.lhs was just an ASTNode so i had to cast it
        return instructions
    }

    // generates the assembly code for an ReadStatementAST node and returns the list of instructions
    fun transReadStatement(node : ReadStatementAST, resultReg: Register) : List<Line> {
        val instructions = mutableListOf<Line>()
        when(node.target.type) {
            IntType -> {
                defineUtilFuncs(P_READ_INT)
                instructions.addAll(transExp(node.target as ExpressionAST, resultReg))
                instructions.add(Move(R0, resultReg))
                instructions.add(BranchLink(P_READ_INT.labelName))
            }
            // complete remaining types...
        }
        return instructions
    }

    // generates assembly code for a VariableDeclarationAST node and returns the list of instructions
    fun transIfBlock(stat: IfBlockAST, resultReg: Register): List<Line> {
        val instructions = mutableListOf<Line>()
        instructions.addAll(transExp(stat.condExpr, resultReg))
        instructions.add(Compare(resultReg, ImmediateOperand(0)))
        val elseLabel = branchLabel.generate()
        instructions.add(Branch(EQ, elseLabel))
        instructions.addAll(transBlock(stat, resultReg))
        val endLabel = branchLabel.generate()
//        instructions.add(BranchLabel(endLabel))
//        instructions.add(BranchLabel(elseLabel))
        instructions.addAll(transBlock(stat.elseBlock as ElseBlockAST, resultReg))
//        instructions.add(BranchLabel(endLabel))
        return instructions
    }

    fun transSequenceStatement(node: SequenceStatementAST, resultReg: Register): List<Line> {
        return transStat(node.stat1, resultReg) + transStat(node.stat2, resultReg)
    }

    //endregion

    // generates the assembly code for an AssignRhsAST node and returns the list of instructions
    fun transAssignRhs(node: AssignRhsAST, resultReg: Register): List<Line> {
        val instructions = mutableListOf<Line>()
        when (node) {
            is ExpressionAST -> instructions.addAll(transExp(node, resultReg))
            is NewPairAST -> instructions.addAll(transNewPair(node, resultReg))
            // complete remaining types...
        }
        return instructions
    }

    // generates assembly code for a NewPairAST node and returns the list of instructions
    fun transNewPair(node : NewPairAST, resultReg: Register) : List<Line> {
        val instructions = mutableListOf<Line>()
        instructions.add(LoadWord(R0, ImmediateOperand(8))) // sets up malloc arg to alloc 8 bytes for the pair
        instructions.add(BranchLink("malloc")) // jump to malloc
        instructions.add(Move(resultReg, R0)) // malloc left alloced address in R0, so here we put this address in resultReg
        instructions.addAll(transExp(node.fstExpr, resultReg.nextReg())) // evaluates the first item of the pair and leaves the result in resultReg + 1
        instructions.add(LoadWord(R0, ImmediateOperand(node.fstExpr.type.sizeInBytes()))) // sets up malloc arg to allocate sufficient bytes for the type of pair.fst
        instructions.add(BranchLink("malloc")) // jumps to malloc
        if (node.fstExpr.type.sizeInBytes() == 4) {
            instructions.add(StoreWord(resultReg.nextReg(), ZeroOffset(R0))) // stores the value of the first item of the pair in the address received from malloc
        } else if (node.fstExpr.type.sizeInBytes() == 1) {
            instructions.add(StoreByte(resultReg.nextReg(), ZeroOffset(R0))) // same as above but for single byte size types
        }
        instructions.add(StoreWord(R0, ZeroOffset(resultReg))) // stores the address of the pair.fst item into the actual pairs memory
        instructions.addAll(transExp(node.sndExpr, resultReg.nextReg())) // evaluates the second item of the pair and leaves the result in resultReg + 1
        instructions.add(LoadWord(R0, ImmediateOperand(node.sndExpr.type.sizeInBytes()))) // sets up malloc arg to allocate sufficient bytes for the type of pair.snd
        instructions.add(BranchLink("malloc")) // jumps to malloc
        if (node.sndExpr.type.sizeInBytes() == 4) {
            instructions.add(StoreWord(resultReg.nextReg(), ZeroOffset(R0))) // stores the value of the second item of the pair in the address received from malloc
        } else if (node.sndExpr.type.sizeInBytes() == 1) {
            instructions.add(StoreByte(resultReg.nextReg(), ZeroOffset(R0))) // same as above but for single byte size types
        }
        instructions.add(StoreWord(resultReg.nextReg(), ImmediateOffset(resultReg,4))) // stores the address of the pair.snd item into the actual pairs memory
        return instructions
    }

    // generates the assembly code for an ExpressionAST node and returns the list of instructions
    fun transExp(expr: ExpressionAST, resultReg: Register): List<Line> {
        val instructions: MutableList<Line> = mutableListOf()
        when (expr) {
            is IntLiteralAST -> {
                instructions.add(LoadWord(resultReg, ImmediateOperand(expr.intValue)))
            }
            is BoolLiteralAST -> {
                instructions.add(Move(resultReg, ImmediateOperand(expr.boolValue)))
            }
            is CharLiteralAST -> {
                instructions.add(Move(resultReg, ImmediateOperand(expr.charValue)))
            }
            is StringLiteralAST -> {
                val label : String = stringLabel.generate()
                data.put(label, StringData(label, expr.stringValue))
                instructions.add(LoadWord(resultReg, LabelOperand(label)))
            }
            is VariableIdentifierAST -> {
                when (expr.type) {
                    IntType -> instructions.add(LoadWord(SP, ImmediateOffset(resultReg,state.getStackPos(expr.varName) - sp)))
                    BoolType -> instructions.add(LoadByte(SP, ImmediateOffset(resultReg,state.getStackPos(expr.varName) - sp)))
                    CharType -> instructions.add(LoadByte(SP, ImmediateOffset(resultReg,state.getStackPos(expr.varName) - sp)))
                    StringType -> instructions.add(LoadWord(SP, ImmediateOffset(resultReg,state.getStackPos(expr.varName) - sp)))
                }
            }
            is BinaryOpExprAST -> instructions.addAll(transBinOp(expr, resultReg))
        }
        return instructions
    }

    // generates the assembly code for a BinaryOpExprAST node and returns the list of instructions
    fun transBinOp(expr: BinaryOpExprAST, resultReg: Register): List<Line> {
        val instructions = mutableListOf<Line>()
        instructions.addAll(transExp(expr.expr1, resultReg))
        instructions.addAll(transExp(expr.expr2, resultReg.nextReg()))
        when (expr.type) {
            IntType -> {
                when (expr.operator) {
                    BinaryOp.MULT -> instructions.add(MultUpdate(resultReg, resultReg, resultReg.nextReg()))
                    // BinaryOp.DIV -> dont rly know whats going on with this one ¯\_(ツ)_/¯
                    // BinaryOp.MOD -> same here, seems like the reference compiler just has a spazm
                    BinaryOp.PLUS -> instructions.add(AddUpdate(resultReg, resultReg, resultReg.nextReg()))
                    // complete remaining operators...
                }
            }
            BoolType -> {
                instructions.addAll(transExp(expr.expr1, resultReg))
                instructions.addAll(transExp(expr.expr2, resultReg.nextReg()))
                when (expr.operator) {
                    BinaryOp.EQUALS -> instructions.addAll(mutableListOf(
                        Compare(resultReg, resultReg.nextReg()),
                        Move(EQ, resultReg, ImmediateOperand(true)),
                        Move(NE, resultReg, ImmediateOperand(false)))
                    )
                    BinaryOp.NOT_EQUALS -> instructions.addAll(mutableListOf(
                        Compare(resultReg, resultReg.nextReg()),
                        Move(NE, resultReg, ImmediateOperand(true)),
                        Move(EQ, resultReg, ImmediateOperand(false)))
                    )
                    // complete remaining operators...
                }
            }
//            CharType -> {
//
//            }
//          .StringType -> {
//
//          }
        }
        return instructions
    }

    fun transUnOp(unOpExpr: UnaryOpExprAST, resultReg: Register): List<Line> {
        val instructions = mutableListOf<Line>()
        instructions.addAll(transExp(unOpExpr.expr, resultReg))

        when (unOpExpr.operator) {
            UnaryOp.BANG -> {
                instructions.add(Xor(resultReg, resultReg, ImmediateOperand(1)))
            }
            UnaryOp.MINUS -> {
                instructions.add(ReverseSub(resultReg, resultReg, ImmediateOperand(0)))
                instructions.add(BranchLink("p_throw_overflow_error"))
//                Add msg to front: "OverflowError: the result is too small/large to store in a 4-byte signed-integer.\n\0"
//                Add msg to front: "%.*s\0"
//                Add to back definitions of the following:
//                p_throw_overflow_error, p_throw_runtime_error, p_print_string
            }
            UnaryOp.LEN -> {
                // instructions.add(LoadWord(resultReg, RegisterOffset(resultReg, false, null)))
            }
            UnaryOp.ORD -> {
//                No actions needed since int ~ char
            }
            UnaryOp.CHR -> {
//                No actions needed since int ~ char
            }
        }

        return instructions
    }

    private fun defineUtilFuncs(vararg funcs: UtilFunction) {
        funcs.forEach { func ->
            if (!utilText.containsKey(func.labelName)) {
                func.dataBlocks.forEach {
                    utilData[it.name] = it
                }
                utilText[func.labelName] = func.labelBlock
            }
        }
    }
}
