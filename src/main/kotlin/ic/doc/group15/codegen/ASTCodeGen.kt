package ic.doc.group15.codegeneration

import ic.doc.group15.ast.*
import ic.doc.group15.codegen.State
import ic.doc.group15.type.BasicType.*
import ic.doc.group15.type.Identifier
import ic.doc.group15.type.Variable
import java.util.*

const val START_VAL = 0

class ASTCodeGen {

    val state : State = State()

    var sp: Int = START_VAL - 1

    companion object {
        // this map stores mappings of labels to the number of words they require for storage and the complete string
        var data: MutableMap<String, Pair<Int, String>> = mutableMapOf()

        // any string literals in the code need to be defined at the top of the assembly file with a unique msg label
        // so this bit of code is just for generating a new unique labels on demand
        var nextStringLabel = 0
        fun nextStringLabel(): String {
            nextStringLabel++
            return "msg_" + (nextStringLabel - 1).toString()
        }

        var nextBranchLabel = 0
        fun nextBranchLabel(): String {
            nextBranchLabel++
            return "L" + (nextBranchLabel - 1).toString()
        }

        val funcDefs : MutableList<Line> = mutableListOf()

        // throughout the program there may be calls to (inbuilt?) functions like p_read_int the implementation of which
        // you can find in the reference compiler. if any of the functions have been called, then we need to add their
        // implementation to our instructions list. to avoid adding them twice, we store a map that tells us which of them
        // have been defined so far
        val defined : MutableMap<String, Boolean> = mutableMapOf(Pair("p_read_int", false)) // add all other predefined functions functions
    }

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
            if (stat is FunctionDeclarationAST) {
                instructions.addAll(transFunctionDeclaration(stat))
            } else if (stat is StatementAST) {
                instructions.addAll(transStat(stat, 4))
            }
        }
        return instructions
    }

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
    fun transBlock(block: BlockAST, resultReg: Int): List<Line> {
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

    // generates the assembly code for a StatementAST node and returns the list of instructions
    fun transStat(stat: StatementAST, resultReg: Int): List<Line> {
        val instructions = mutableListOf<Line>()
        when (stat) {
            is SkipStatementAST -> {}
            is VariableDeclarationAST -> instructions.addAll(transVarDeclaration(stat, resultReg))
            is AssignToIdentAST -> instructions.addAll(transVarAssignToIdent(stat, resultReg))
            is ReadStatementAST -> instructions.addAll(transReadStatement(stat, resultReg)) // needs to add instructions to the end
            is IfBlockAST -> instructions.addAll(transIfBlock(stat, resultReg))
            is BlockAST -> instructions.addAll(transBlock(stat, resultReg))
            // complete remaining statement types...
        }
        return instructions
    }

    // generates assembly code for a VariableDeclarationAST node and returns the list of instructions
    fun transVarDeclaration(node: VariableDeclarationAST, resultReg: Int): List<Line> {
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
    fun transVarAssignToIdent(node: AssignToIdentAST, resultReg: Int): List<Line> {
        val instructions = mutableListOf<Line>()
        instructions.addAll(transAssignRhs(node.rhs, resultReg))
        instructions.add(STRsp(resultReg, state.getStackPos((node.lhs as VariableIdentifierAST).varName) - sp)) // dk y but it assumed node.lhs was just an ASTNode so i had to cast it
        return instructions
    }

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

    // returns the code for the p_read_int instruction
    fun define_p_read_int() {
        val label = nextStringLabel()
        data.put(label, Pair(3, "%d\\0"))
        funcDefs.addAll(mutableListOf(
            Label("p_read_int"),
            PUSHlr(),
            MOVreg(1, 0),
            LDRimmString(0, label),
            ADD(0, 0, 5),
            BL("scanf"),
            POPpc()))
        defined["p_read_int"] = true
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
                val label : String = nextStringLabel()
                data.put(label, Pair(expr.stringValue.length, expr.stringValue))
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
//          .CharType -> {
//
//          }
//          .StringType -> {
//
//          }
        }
        return instructions
    }
}
