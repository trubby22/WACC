package ic.doc.group15.codegen

import ic.doc.group15.ast.*
import ic.doc.group15.codegen.assembly.*
import ic.doc.group15.codegen.assembly.UtilFunction.P_READ_INT
import ic.doc.group15.codegen.State
import ic.doc.group15.codegen.assembly.instruction.LoadWord
import ic.doc.group15.codegen.assembly.operand.ImmediateOperand
import ic.doc.group15.codegen.assembly.operand.RegisterOffset
import ic.doc.group15.instructions.ImmOp
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
                when (i.type) {
                    IntType -> stackSpace += 4
                    BoolType -> stackSpace += 1
                    CharType -> stackSpace += 1
                    StringType -> stackSpace += 4
                }
            }
        }
        return stackSpace
    }

    fun transProgram(program: BlockAST) : List<Line> {
        val instructions = mutableListOf<Line>()
        val statements: List<StatementAST> = program.statements
        for (stat in statements) {
            instructions.addAll(transStat(stat, 4))
        }
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
            when (i.type) {
                IntType -> pos += 4
                BoolType -> pos += 1
                CharType -> pos += 1
                StringType -> pos += 4
            }
        }
        val stackSpace = requiredStackSpace(funcDec) - pos // the required stack space function will take into account the parameters as they are part of the symbol table for functions but we have already taken these into account
        sp-= stackSpace
        instructions.addAll(mutableListOf(Label("f_" + funcDec.funcName),
                                          PUSHlr(),
                                          SUBspSpImm(stackSpace)))
        instructions.addAll(transBlock((funcDec) as BlockAST, 4))
        instructions.add(MOVreg(0, 4))
        instructions.add(ADDspSpImm(stackSpace))
        instructions.add(POPpc())
        instructions.add(POPpc())
        instructions.add(LTORG())
        return instructions
    }

    fun transCall(call : CallAST, resultReg: Int) {
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
            var decrement = 0
            when(i.type) {
                IntType -> decrement = 4
                BoolType -> decrement = 1
                CharType -> decrement = 1
                StringType -> decrement = 4
            }
            sp -= decrement
            spDec += decrement
            instructions.addAll(transExp(i, resultReg))
            instructions.add(STRspMov(resultReg, -decrement))
        }
        instructions.add(BL("p_" + call.funcName))
        instructions.add(ADDspsp(spDec))
        instructions.add(MOVreg(resultReg, 0))
    }

    // generates the assembly code for a BlockAST node and returns the list of instructions
    fun transStat(block: BlockAST, resultReg: Int): List<Line> {
        val instructions = mutableListOf<Line>()
        var stackSpace = requiredStackSpace(block)
        sp -= stackSpace
        instructions.add(SUBspSpImm(stackSpace))
        val statements: List<StatementAST> = block.statements
        for (stat in statements) {
            instructions.addAll(transStat(stat, resultReg))
        }
        sp += stackSpace
        instructions.add(ADDspSpImm(stackSpace))
        state.popStacks(block.symbolTable.getMap().keys.parallelStream().toList()) // read comments in the State class to understand why we do this
        return instructions
    }

    // generates assembly code for a VariableDeclarationAST node and returns the list of instructions
    fun transStat(node: VariableDeclarationAST, resultReg: Int): List<Line> {
        val instructions = mutableListOf<Line>()
        var spDecrement = 0
        when (node.varIdent.type) {
            IntType -> spDecrement = 4
            BoolType -> spDecrement = 1
            CharType -> spDecrement = 1
            StringType -> spDecrement = 4
        }
        sp -= spDecrement
        state.setStackPos(node.varName, sp)
        instructions.addAll(transAssignRhs(node.rhs, resultReg))
        instructions.add(STRsp(resultReg, state.getStackPos(node.varName)))
        return instructions
    }

    // this function will generate the assembly code for an AssignToIdentAST node and return the list of instructions
    fun transStat(node: AssignToIdentAST, resultReg: Int): List<Line> {
        val instructions = mutableListOf<Line>()
        instructions.addAll(transAssignRhs(node.rhs, resultReg))
        instructions.add(STRsp(resultReg, state.getStackPos((node.lhs as VariableIdentifierAST).varName) - sp)) // dk y but it assumed node.lhs was just an ASTNode so i had to cast it
        return instructions
    }

    // generates the assembly code for an ReadStatementAST node and returns the list of instructions
    fun transStat(node : ReadStatementAST, resultReg: Int) : List<Line> {
        val instructions = mutableListOf<Line>()
        when(node.target.type) {
            IntType -> {
                defineUtilFuncs(P_READ_INT)
                instructions.add(ADDspImm(resultReg, 0))
                instructions.add(MOVreg(0, resultReg))
                instructions.add(BL(P_READ_INT.labelName))
            }
            // complete remaining types...
        }
        return instructions
    }

    fun transStat(node: SequenceStatementAST, resultReg: Int): List<Line> {
        return transStat(node.stat1, resultReg + 1) + transStat(node.stat2,
            resultReg + 2)
    }

    // generates assembly code for a VariableDeclarationAST node and returns the list of instructions
    fun transBlock(ifBlock: IfBlockAST, resultReg: Int): List<Line> {
        val instructions = mutableListOf<Line>()
        instructions.addAll(transExp(ifBlock.condExpr, resultReg))
        instructions.add(CMPimm(resultReg, 0))
        val elseLabel = branchLabel.generate()
        instructions.add(BEQ(elseLabel))
        for (stat in ifBlock.statements) {
            instructions.addAll(transStat(stat, resultReg))
        }
        val endLabel = branchLabel.generate()
        instructions.add(B(endLabel))
        instructions.add(Label(elseLabel))
        instructions.addAll(transStat(ifBlock.elseStat, resultReg))
        instructions.add(Label(endLabel))
        return instructions
    }

    //endregion

    // generates the assembly code for an AssignRhsAST node and returns the list of instructions
    fun transAssignRhs(node: AssignRhsAST, resultReg: Int): List<Line> {
        val instructions = mutableListOf<Line>()
        when (node) {
            is ExpressionAST -> instructions.addAll(transExp(node, resultReg))
            // complete remaining types...
        }
        return instructions
    }

    // generates the assembly code for an ReadStatementAST node and returns the list of instructions
    fun transReadStatement(node : ReadStatementAST, resultReg: Int) : List<Line> {
        val instructions = mutableListOf<Line>()
        when(node.target.type) {
            IntType -> {
                instructions.add(ADDspImm(resultReg, 0))
                instructions.add(MOVreg(0, resultReg))
                instructions.add(BL("p_read_int"))
                define_p_read_int()
            }
            // complete remaining types...
        }
        return instructions
    }

    // generates assembly code for a VariableDeclarationAST node and returns the list of instructions
    fun transIfBlock(stat: IfBlockAST, resultReg: Int): List<Line> {
        val instructions = mutableListOf<Line>()
        instructions.addAll(transExp(stat.condExpr, resultReg))
        instructions.add(CMPimm(resultReg, 0))
        val elseLabel = nextBranchLabel()
        instructions.add(BEQ(elseLabel))
        instructions.addAll(transStat(stat.thenStat, resultReg))
        val endLabel = nextBranchLabel()
        instructions.add(B(endLabel))
        instructions.add(Label(elseLabel))
        instructions.addAll(transStat(stat.elseStat, resultReg))
        instructions.add(Label(endLabel))
        return instructions
    }

    // generates the assembly code for an ExpressionAST node and returns the list of instructions
    fun transExp(expr: ExpressionAST, resultReg: Int): List<Line> {
        val instructions: MutableList<Line> = mutableListOf()
        when (expr) {
            is IntLiteralAST -> {
                instructions.add(LDRimmInt(resultReg, expr.intValue))
            }
            is BoolLiteralAST -> {
                instructions.add(MOVimmBool(resultReg, expr.boolValue))
            }
            is CharLiteralAST -> {
                instructions.add(MOVimmChar(resultReg, expr.charValue))
            }
            is StringLiteralAST -> {
                val label : String = stringLabel.generate()
                data.put(label, StringData(label, expr.stringValue))
                instructions.add(LDRimmString(resultReg, label))
            }
            is VariableIdentifierAST -> {
                when (expr.type) {
                    IntType -> instructions.add(LDRsp(resultReg, state.getStackPos(expr.varName) - sp))
                    BoolType -> instructions.add(LDRSBsp(resultReg, state.getStackPos(expr.varName) - sp))
                    CharType -> instructions.add(LDRSBsp(resultReg, state.getStackPos(expr.varName) - sp))
                    StringType -> instructions.add(LDRsp(resultReg, state.getStackPos(expr.varName) - sp))
                }
            }
            is BinaryOpExprAST -> instructions.addAll(transBinOp(expr, resultReg))
        }
        return instructions
    }

    // generates the assembly code for a BinaryOpExprAST node and returns the list of instructions
    fun transBinOp(expr: BinaryOpExprAST, resultReg: Int): List<Line> {
        val instructions = mutableListOf<Line>()
        instructions.addAll(transExp(expr.expr1, resultReg))
        instructions.addAll(transExp(expr.expr2, resultReg + 1))
        when (expr.type) {
            IntType -> {
                when (expr.operator) {
                    BinaryOp.MULT -> instructions.add(SMULL(resultReg, resultReg + 1, resultReg, resultReg + 1))
                    // BinaryOp.DIV -> dont rly know whats going on with this one ¯\_(ツ)_/¯
                    // BinaryOp.MOD -> same here, seems like the reference compiler just has a spazm
                    BinaryOp.PLUS -> instructions.add(ADDS(resultReg, resultReg, resultReg + 1))
                    // complete remaining operators...
                }
            }
            BoolType -> {
                instructions.addAll(transExp(expr.expr1, resultReg))
                instructions.addAll(transExp(expr.expr2, resultReg + 1))
                when (expr.operator) {
                    BinaryOp.EQUALS -> instructions.addAll(mutableListOf(CMPimm(resultReg, resultReg + 1), MOVEQ(resultReg, true), MOVNE(resultReg, false)))
                    BinaryOp.NOT_EQUALS -> instructions.addAll(mutableListOf(CMPimm(resultReg, resultReg + 1), MOVNE(resultReg, true), MOVEQ(resultReg, false)))
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

    fun transUnOp(unOpExpr: UnaryOpExprAST, resultReg: Int): List<Line> {
        val instructions = mutableListOf<Line>()
        instructions.addAll(transExp(unOpExpr.expr, resultReg))

        when (unOpExpr.operator) {
            UnaryOp.BANG -> {
                instructions.add(EOR(resultReg, resultReg, ImmediateOperand(1)))
            }
            UnaryOp.MINUS -> {
                instructions.add(RSB(resultReg, resultReg, ImmediateOperand(0)))
                instructions.add(BL("p_throw_overflow_error"))
//                Add msg to front: "OverflowError: the result is too small/large to store in a 4-byte signed-integer.\n\0"
//                Add msg to front: "%.*s\0"
//                Add to back definitions of the following:
//                p_throw_overflow_error, p_throw_runtime_error, p_print_string
            }
            UnaryOp.LEN -> {
                instructions.add(LoadWord(resultReg, RegisterOffset(resultReg, false, null)))
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
