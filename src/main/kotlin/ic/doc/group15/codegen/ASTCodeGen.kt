package ic.doc.group15.codegeneration

import com.sun.jdi.BooleanType
import ic.doc.group15.semantics.*
import ic.doc.group15.semantics.ast.*

class ASTCodeGen {

    var sp : Int = Int.MAX_VALUE;

    companion object {
        // this map stores mappings of labels to the number of words they require for storage and the complete string
        var data : MutableMap<String, Pair<Int, String>> = mutableMapOf()
        var nextLabelNum = 0
        fun nextLabelNum() : Int {
            nextLabelNum++
            return nextLabelNum - 1
        }
    }

    fun createStackSpace(node : BlockAST) : Int {
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
        sp -= stackSpace
        return stackSpace
        // check symbol table variables
        // to see how much to move sp
        // can then store the variables positions in the stack relative to the stack pointer in the symbol table (do here or in assign visitgen)
        // do variable register colouring here? idk tho cus this is a tree and for so we will need to be able to see the order of variable use within the scope
    }

    fun blockGen(block : BlockAST) {
        var nextRelativeStackSlot = createStackSpace(block)
        val statements : List<StatementAST> = block.statements
        for (stat in statements) {
            when (stat) {
                is BlockAST -> blockGen(stat)
                is VariableDeclarationAST -> {
                    when (stat.varIdent.type) {
                        BasicType.IntType -> { nextRelativeStackSlot -= 4
                                               stat.varIdent.stackPos = sp + nextRelativeStackSlot
                                               assignToIntIdentDeclarationGen(stat) }
                        BasicType.BoolType -> { nextRelativeStackSlot -= 1
                                                stat.varIdent.stackPos = sp + nextRelativeStackSlot
                                                assignToBoolIdentDeclarationGen(stat) }
                        BasicType.CharType -> { nextRelativeStackSlot -= 1
                                                stat.varIdent.stackPos = sp + nextRelativeStackSlot
                                                assignToCharIdentDeclarationGen(stat) }
                        BasicType.StringType -> { nextRelativeStackSlot -= 4
                                                  stat.varIdent.stackPos = sp + nextRelativeStackSlot
                                                  assignToStringIdentDeclarationGen(stat) }
                    }
                    // need to go over rest of statement types
                }
                is AssignToIdentAST -> {
                    when (stat.type) {
                        BasicType.IntType -> assignToIntIdentGen(stat)
                        BasicType.BoolType -> assignToBoolIdentGen(stat)
                        BasicType.CharType -> assignToCharIdentGen(stat)
                        BasicType.StringType -> assignToStringIdentGen(stat)
                    }
                }
            }
        }
    }

    // need to evaluate the rhs of expressions before making assignment
    fun evaluateIntExp() : Int {
        return 0
    }

    fun evaluateBoolExp() : Boolean {
        return true
    }

    fun evaluateCharExp() : Char {
        return 'a'
    }

    fun evaluateStringExp() : String {
        return "yep"
    }

    fun assignToIntIdentDeclarationGen(node: VariableDeclarationAST) : List<Instr> {
        val rhsVal = evaluateIntExp(/* the right hand side of the expression wherever it is */)
        val loadInstr = LDRimmInt(4, rhsVal) 
        val storeInstr = STRsp(4, node.varIdent.stackPos)
        return mutableListOf(loadInstr, storeInstr);
    }

    fun assignToBoolIdentDeclarationGen(node: VariableDeclarationAST) : List<Instr> {
        val rhsVal = evaluateBoolExp(/* the right hand side of the expression wherever it is */)
        val moveInstr = MOVimmBool(4, rhsVal)
        val storeInstr = STRBsp(4, node.varIdent.stackPos)
        return mutableListOf(moveInstr, storeInstr);
    }

    fun assignToCharIdentDeclarationGen(node: VariableDeclarationAST) : List<Instr> {
        val rhsVal = evaluateCharExp(/* the right hand side of the expression wherever it is */)
        val moveInstr = MOVimmChar(4, rhsVal)
        val storeInstr = STRBsp(4, node.varIdent.stackPos)
        return mutableListOf(moveInstr, storeInstr);
    }

    fun assignToStringIdentDeclarationGen(node: VariableDeclarationAST) : List<Instr> {
        val rhsVal = evaluateStringExp(/* the right hand side of the expression wherever it is */)
        val label : String = "msg_" + nextLabelNum().toString()
        data.put(label, Pair(rhsVal.length, rhsVal))
        val loadInstr = LDRimmString(4, label)
        val storeInstr = STRsp(4, node.varIdent.stackPos)
        return mutableListOf(loadInstr, storeInstr);
    }

    fun assignToIntIdentGen(node: AssignToIdentAST) : List<Instr> {
        //val identEntry = node.symbolTable.lookup(node.toString()) as IntVariable
        val rhsVal: Int = evaluateIntExp(/* the right hand side of the expression wherever it is */)
        val loadInstr = LDRimmInt(4, rhsVal) // put garbage placeholder register and stack pointer values for now
        val storeInstr = STRsp(4, (node.lhs as VariableIdentifierAST).ident.stackPos)
        return mutableListOf(loadInstr, storeInstr);
    }

    fun assignToBoolIdentGen(node: AssignToIdentAST) : List<Instr> {
        val rhsVal: Boolean = evaluateBoolExp(/* the right hand side of the expression wherever it is */)
        val moveInstr = MOVimmBool(4, rhsVal)
        val storeInstr = STRBsp(4, (node.lhs as VariableIdentifierAST).ident.stackPos)
        return mutableListOf(moveInstr, storeInstr);
    }

    fun assignToCharIdentGen(node: AssignToIdentAST) : List<Instr> {
        val rhsVal: Char = evaluateCharExp(/* the right hand side of the expression wherever it is */)
        val moveInstr = MOVimmChar(4, rhsVal)
        val storeInstr = STRBsp(4, (node.lhs as VariableIdentifierAST).ident.stackPos)
        return mutableListOf(moveInstr, storeInstr);
    }

    fun assignToStringIdentGen(node: AssignToIdentAST) : List<Instr> {
        val rhsVal: String = evaluateStringExp(/* the right hand side of the expression wherever it is */)
        val label : String = "msg_" + nextLabelNum().toString()
        data.put(label, Pair(rhsVal.length, rhsVal))
        val loadInstr = LDRimmString(4, label)
        val storeInstr = STRsp(4, (node.lhs as VariableIdentifierAST).ident.stackPos)
        return mutableListOf(loadInstr, storeInstr);
    }

    // so basically, declaration statements are in the AST as VariableDeclarationAST nodes and have an AssignRhsAST subchild for what it is assigned to
    // and assignment statements are in the AST as AssigmentAST nodes and have an AssignRhsAST subchild for what it is assigned to
    // AssignToIdent is a sub class of AssignmentAST for assigning to variables and is for when the variable already exists, otherwise it would be a VariableDeclarationAST but theres also AssignToArrayElemAST and AssignToPairElemAST for assigning to arrays and pairs
}


