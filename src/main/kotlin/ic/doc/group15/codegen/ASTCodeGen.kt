package ic.doc.group15.codegeneration

import com.sun.jdi.BooleanType
import com.sun.jdi.IntegerType
import ic.doc.group15.semantics.*
import ic.doc.group15.semantics.ast.*

const val STACK_SIZE = 10000

class ASTCodeGen {

    var sp : Int = STACK_SIZE - 1

    companion object {
        // this map stores mappings of labels to the number of words they require for storage and the complete string
        var data : MutableMap<String, Pair<Int, String>> = mutableMapOf()

        // any string literals in the code need to be defined at the top of the assembly file with a unique msg label
        // so this bit of code is just for generating a new unique labels on demand
        var nextStringLabel = 0
        fun nextStringLabel() : String {
            nextStringLabel++
            return "msg_" + (nextStringLabel - 1).toString()
        }

        var nextBranchLabel = 0
        fun nextBranchLabel() : String {
            nextBranchLabel++
            return "L" + (nextBranchLabel - 1).toString()
        }
    }

    // when we enter a block, we will prematurely calculate how much stack space that block will need by summing the
    // size in bytes of each of the variables in its symbol table. then we will be able to decrement the stack pointer
    // by this amount in one go leaving enough space for the execution of the entire block. the below function takes
    // in a block and returns the amount of stack space it will require
    fun requiredStackSpace(node : BlockAST) : Int {
        var stackSpace = 0
        val st = node.symbolTable
        val map = st.getMap()
        val list : List<Identifier> = map.values as List<Identifier>
        for (i in list) {
            if (i is Variable) {
                when (i.type) {
                    BasicType.IntType -> stackSpace += 4
                    BasicType.BoolType -> stackSpace += 1
                    BasicType.CharType -> stackSpace += 1
                    BasicType.StringType -> stackSpace += 4
                }
            }
        }
        return stackSpace
    }

    // generates the assembly code for a BlockAST node and returns the list of instructions
    fun transBlock(block : BlockAST, resultReg: Int) : List<Line> {
        val instructions = mutableListOf<Line>()
        var stackSpace = requiredStackSpace(block)
        sp -= stackSpace
        val statements : List<StatementAST> = block.statements
        for (stat in statements) {
            transStat(stat, resultReg)
        }
        sp += stackSpace
        return instructions
    }

    // generates the assembly code for a StatementAST node and returns the list of instructions
    fun transStat(stat : StatementAST, resultReg: Int) : List<Line> {
        val instructions = mutableListOf<Line>()
        when (stat) {
            is BlockAST -> instructions.addAll(transBlock(stat, resultReg))
            is VariableDeclarationAST -> instructions.addAll(transVarDeclaration(stat, resultReg))
            is AssignToIdentAST -> instructions.addAll(transVarAssignToIdent(stat, resultReg))
            is IfBlockAST -> instructions.addAll(transIfBlock(stat, resultReg))
            // complete remaining statement types...
        }
        return instructions
    }

    // generates assembly code for a VariableDeclarationAST node and returns the list of instructions
    fun transVarDeclaration(node: VariableDeclarationAST, resultReg: Int) : List<Line> {
        val instructions = mutableListOf<Line>()
        var spDecrement = 0
        when (node.varIdent.type) {
            BasicType.IntType -> spDecrement = 4
            BasicType.BoolType -> spDecrement = 1
            BasicType.CharType -> spDecrement = 1
            BasicType.StringType -> spDecrement = 4
        }
        sp -= spDecrement
        node.varIdent.stackPos = sp
        instructions.add(STRsp(resultReg, node.varIdent.stackPos))
        return instructions
    }

    // this function will generate the assembly code for an AssignToIdentAST node and return the list of instructions
    fun transVarAssignToIdent(node: AssignToIdentAST, resultReg: Int) : List<Line> {
        val instructions = mutableListOf<Line>()
        instructions.addAll(transAssignRhs(node.rhs, resultReg))
        instructions.add(STRsp(resultReg, (node.lhs as VariableIdentifierAST).ident.stackPos)) // dk y but it assumed node.lhs was just an ASTNode so i had to cast it
        return instructions
    }

    // generates the assembly code for an AssignRhsAST node and returns the list of instructions
    fun transAssignRhs(node: AssignRhsAST, resultReg: Int) : List<Line> {
        val instructions = mutableListOf<Line>()
        when (node) {
            is ExpressionAST -> instructions.addAll(transExp(node, resultReg))
            // complete remaining types...
        }
        return instructions
    }

    // generates assembly code for a VariableDeclarationAST node and returns the list of instructions
    fun transIfBlock(stat : IfBlockAST, resultReg: Int) : List<Line> {
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
    fun transExp(expr: ExpressionAST, resultReg: Int) : List<Line> {
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
                val label = nextStringLabel()
                data.put(label, Pair(expr.stringValue.length, expr.stringValue))
                instructions.add(LDRimmString(resultReg, label))
            }
            is VariableIdentifierAST -> {
                when (expr.type) {
                    BasicType.IntType -> instructions.add(LDRsp(resultReg, expr.ident.stackPos - sp))
                    BasicType.BoolType -> instructions.add(LDRSBsp(resultReg, expr.ident.stackPos - sp))
                    BasicType.CharType -> instructions.add(LDRSBsp(resultReg, expr.ident.stackPos - sp))
                    BasicType.StringType -> instructions.add(LDRsp(resultReg, expr.ident.stackPos - sp))
                }
            }
            is BinaryOpExprAST -> instructions.addAll(transBinOp(expr, resultReg))
        }
        return instructions
    }

    // generates the assembly code for a BinaryOpExprAST node and returns the list of instructions
    fun transBinOp(expr: BinaryOpExprAST, resultReg : Int) : List<Line> {
        val instructions = mutableListOf<Line>()
        instructions.addAll(transExp(expr.expr1, resultReg))
        instructions.addAll(transExp(expr.expr2, resultReg + 1))
        when (expr.type) {
            BasicType.IntType -> {
                when (expr.operator) {
                    BinaryOp.MULT -> instructions.add(SMULL(resultReg, resultReg + 1, resultReg, resultReg + 1))
                    // BinaryOp.DIV -> dont rly know whats going on with this one ¯\_(ツ)_/¯
                    // BinaryOp.MOD -> same here, seems like the reference compiler just has a spazm
                    BinaryOp.PLUS -> instructions.add(ADDS(resultReg, resultReg, resultReg + 1))
                    // complete remaining operators...

                }
            }
            BasicType.BoolType -> {
                instructions.addAll(transExp(expr.expr1, resultReg))
                instructions.addAll(transExp(expr.expr2, resultReg + 1))
                when (expr.operator) {
                    BinaryOp.EQUALS -> instructions.addAll(mutableListOf(CMPimm(resultReg, resultReg + 1), MOVEQ(resultReg, true), MOVNE(resultReg, false)))
                    BinaryOp.NOT_EQUALS -> instructions.addAll(mutableListOf(CMPimm(resultReg, resultReg + 1), MOVNE(resultReg, true), MOVEQ(resultReg, false)))
                    // complete remaining operators...
                }
            }
//          BasicType.CharType -> {
//
//          }
//          BasicType.StringType -> {
//
//          }
        }
        return instructions
    }
}


