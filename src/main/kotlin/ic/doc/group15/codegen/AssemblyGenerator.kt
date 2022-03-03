package ic.doc.group15.codegen

import ic.doc.group15.SymbolTable
import ic.doc.group15.WORD
import ic.doc.group15.ast.*
import ic.doc.group15.codegen.assembly.*
import ic.doc.group15.codegen.assembly.LibraryFunction.Companion.AEABI_IDIV
import ic.doc.group15.codegen.assembly.LibraryFunction.Companion.AEABI_IDIVMOD
import ic.doc.group15.codegen.assembly.LibraryFunction.Companion.MALLOC
import ic.doc.group15.codegen.assembly.LibraryFunction.Companion.PUTCHAR
import ic.doc.group15.codegen.assembly.UtilFunction.*
import ic.doc.group15.codegen.assembly.instruction.*
import ic.doc.group15.codegen.assembly.instruction.ConditionCode.*
import ic.doc.group15.codegen.assembly.operand.*
import ic.doc.group15.codegen.assembly.operand.Register.*
import ic.doc.group15.type.ArrayType
import ic.doc.group15.type.BasicType
import ic.doc.group15.type.BasicType.*
import ic.doc.group15.type.FunctionType
import ic.doc.group15.type.PairType

const val START_VAL = 0

class AssemblyGenerator(private val ast: AST, private val st: SymbolTable) {

    private val state : State = State()

    private var sp: Int = START_VAL - 1

    private lateinit var currentLabel: BranchLabel

    /**
     * Represents the ".dataLabel" section of the assembly code.
     *
     * Contains info for raw dataLabel in memory, such as string literals.
     */
    private val data: MutableMap<String, DataLabel> = mutableMapOf()
    private val utilData: MutableMap<String, DataLabel> = mutableMapOf()

    /**
     * Represents the ".text" section of the assembly code.
     *
     * Contains labels that can be branched to, and the main function.
     */
    private val text: MutableMap<String, BranchLabel> = mutableMapOf()
    private val utilText: MutableMap<String, BranchLabel> = mutableMapOf()

    private val stringLabelGenerator = UniqueStringLabelGenerator()
    private val branchLabelGenerator = UniqueBranchLabelGenerator()

    fun generate(): String {
        var asm = ""
        if (data.isNotEmpty() || utilData.isNotEmpty()) {
            asm += ".data\n\n"
        }
        asm += joinAsm(data.values) +
               joinAsm(utilData.values) +
               ".text\n\n.global main\n" +
               joinAsm(text.values) +
               joinAsm(utilText.values)
        return asm
    }

    private fun joinAsm(asm: Collection<Assembly>): String {
        return asm.joinToString(separator = "\n", postfix = if (asm.isNotEmpty()) "\n" else "")
    }

    /**
     * Per WACC language specification, a program matches the grammar "begin func* stat end".
     * The AST representation decouples the statements from a SequenceAST to a mapping of lists of StatementAST
     * in the symbol table to avoid stack overflow from recursing a huge block of statements.
     *
     * transProgram dissolves the program into a list of functions, pass the FunctionDeclarationAST
     * into transFunction, and add the list of statements of the main program into a FunctionDeclarationAST,
     * thereby processing it via transFunction as well.
     */
    @Translator(AST::class)
    private fun transProgram(program: AST) {
        val functionASTs = program.statements.filterIsInstance<FunctionDeclarationAST>()

        // translate all function blocks into assembly
        functionASTs.map { f -> f as FunctionDeclarationAST }
            .forEach { f -> transFunctionDeclaration(f) }

        // Translate main instructions into assembly
        // We create a new FunctionDeclarationAST to store statements in the main function
        // and let transFunction add the entries to the text attribute

        // The symbol table contains the statements AND the FunctionDeclarationAST
        // For future proofing, we might want to remove all FunctionDeclarationASTs when
        // nested function definition is introduced in the extension task
        val mainAST = FunctionDeclarationAST(program, program.symbolTable, IntType, "main")
        mainAST.funcIdent = FunctionType(IntType, emptyList(), program.symbolTable)
        mainAST.returnStat = ReturnStatementAST(mainAST, mainAST.symbolTable.subScope(), IntLiteralAST(0))
        transFunctionDeclaration(mainAST)
    }

    // TODO: Implement
    @Translator(FunctionDeclarationAST::class)
    private fun transFunctionDeclaration(funcDec: FunctionDeclarationAST) {
//        val instructions = mutableListOf<Line>()
//        var pos = 0
//        for (i in funcDec.formals) {
//            state.setStackPos(i.varName, sp + pos)
//            pos += i.type.sizeInBytes()
//        }
//        val stackSpace =
//            requiredStackSpace(funcDec) - pos // the required stack space function will take into account the parameters as they are part of the symbol table for functions but we have already taken these into account
//        sp -= stackSpace
//        // functions are missing labels for now as well as .ltorg at the end
//        instructions.addAll(
//            mutableListOf(
//                Push(LR),
//                Sub(SP, SP, ImmediateOperand(stackSpace))
//            )
//        )
//        instructions.addAll(transBlock((funcDec) as BlockAST, R4))
//        instructions.add(Move(R0, R4))
//        instructions.add(Add(SP, SP, ImmediateOperand(stackSpace)))
//        instructions.add(Pop(PC))
    }

    // TODO: Implement
    @Translator(CallAST::class)
    private fun transCall(call: CallAST, resultReg: Register) {
//        // parameters will be put onto the stack in reverse order e.g. f(a, b, c)
//        val revParams = call.actuals.toMutableList().reversed()
//        var spDec = 0
//        for (i in revParams) {
//            sp -= i.type.sizeInBytes()
//            spDec += i.type.sizeInBytes()
//            instructions.addAll(transExp(i, resultReg))
//            instructions.add(Sub(SP, SP, ImmediateOperand(-i.type.sizeInBytes())))
//            instructions.add(StoreWord(SP, ZeroOffset(resultReg)))
//        }
//        instructions.add(BranchLink("p_" + call.funcName))
//        instructions.add(Add(SP, SP, ImmediateOperand(spDec)))
//        instructions.add(Move(resultReg, R0))
    }

    @Translator(VariableDeclarationAST::class)
    private fun transVariableDeclaration(node: VariableDeclarationAST, resultReg: Register) {
        val instructions = mutableListOf<Line>()
        sp -= node.varIdent.type.sizeInBytes()
        state.setStackPos(node.varName, sp)
        transAssignRhs(node.rhs, resultReg)
        addLines(
            StoreWord(SP, ImmediateOffset(resultReg, state.getStackPos(node.varName)))
        )
    }

    @Translator(AssignToIdentAST::class)
    private fun transAssignToIdent(node: AssignToIdentAST, resultReg: Register) {
        transAssignRhs(node.rhs, resultReg)
        addLines(
            StoreWord(
                SP,
                ImmediateOffset(resultReg, state.getStackPos(node.lhs.varName) - sp)
            )
        )
    }

    // TOOD: Implement
    @Translator(AssignToArrayElemAST::class)
    private fun transAssignToArrayElem(node: AssignToArrayElemAST, resultReg: Register) {

    }

    // TODO: Implement
    @Translator(AssignToPairElemAST::class)
    private fun transAssignToPairElem(node: AssignToPairElemAST, resultReg: Register) {

    }

    @Translator(ArrayLiteralAST::class)
    private fun transArrayLiteral(node: ArrayLiteralAST, resultReg: Register) {
        val size = 4 + (node.elems.size * (node.elems[0].type.sizeInBytes())) // calculate bytes need to malloc
        currentLabel.addLines(
            LoadWord(R0, ImmediateOperand(size)),
            BranchLink(MALLOC),
            Move(resultReg, R0)
        )
        var offset = 0 // now we go in this for loop to put all the items of the array into the memory of the array
        for (expr in node.elems) {
            transExp(expr, resultReg.nextReg())
            if (expr.type.sizeInBytes() == 4) {
                offset += 4
                currentLabel.addLines(
                    StoreWord(resultReg.nextReg(), ImmediateOffset(resultReg, offset))
                )
            } else {
                offset += 1
                currentLabel.addLines(
                    StoreByte(resultReg.nextReg(), ImmediateOffset(resultReg, offset))
                )
            }
        }
        currentLabel.addLines(
            LoadWord(resultReg.nextReg(), ImmediateOperand(node.elems.size)),
            StoreWord(resultReg.nextReg(), ZeroOffset(resultReg))
        )
    }

    @Translator(FstPairElemAST::class)
    fun transFstPairElem(node: FstPairElemAST, resultReg: Register) {

    }

    @Translator(SndPairElemAST::class)
    fun transSndPairElem(node: SndPairElemAST, resultReg: Register) {

    }

    @Translator(ElseBlockAST::class)
    fun transElseBlock(node: ElseBlockAST, resultReg: Register) {

    }

    @Translator(WhileBlockAST::class)
    fun transWhileBlock(node: WhileBlockAST, resultReg: Register) {

    }

    @Translator(BeginEndBlockAST::class)
    fun transBeginEndBlock(node: BeginEndBlockAST, resultReg: Register) {

    }

    @Translator(ArrayElemAST::class)
    fun transArrayElem(node: ArrayElemAST, resultReg: Register) {

    }

    @Translator(ParameterAST::class)
    fun transParameter(node: ParameterAST, resultReg: Register) {

    }

    @Translator(FreeStatementAST::class)
    fun transFreeStatement(node: FreeStatementAST, resultReg: Register) {

    }

    @Translator(ReturnStatementAST::class)
    fun transReturnStatement(node: ReturnStatementAST, resultReg: Register) {

    }

    @Translator(ExitStatementAST::class)
    fun transExitStatement(node: ExitStatementAST, resultReg: Register) {

    }

    @Translator(PrintStatementAST::class)
    fun transPrintStatement(node: PrintStatementAST, resultReg: Register) {
        transExp(node.expr, resultReg)
        addLines(
            Move(R0, resultReg)
        )
        when (node.expr.type) {
            StringType -> {
                defineUtilFuncs(P_PRINT_STRING)
                addLines(BranchLink(P_PRINT_STRING))
            }
            CharType -> {
                addLines(BranchLink(PUTCHAR))
            }
            IntType -> {
                defineUtilFuncs(P_PRINT_INT)
                addLines(BranchLink(P_PRINT_INT))
            }
            BoolType -> {
                defineUtilFuncs(P_PRINT_BOOL)
                addLines(BranchLink(P_PRINT_BOOL))
            }
            is PairType -> {
                defineUtilFuncs(P_PRINT_REFERENCE)
                addLines(BranchLink(P_PRINT_REFERENCE))
            }
            is ArrayType -> {
                defineUtilFuncs(P_PRINT_REFERENCE)
                addLines(BranchLink(P_PRINT_REFERENCE))
            }
        }
    }

    @Translator(PrintlnStatementAST::class)
    fun transPrintlnStatement(
        node: PrintlnStatementAST, resultReg:
        Register
    ) {
        val printStatementAST = PrintStatementAST(node.parent!!, node.symbolTable, node.expr)
        transPrintStatement(printStatementAST, resultReg)
        addLines(BranchLink(P_PRINT_LN))
    }

    @Translator(ReadStatementAST::class)
    fun transReadStatement(node: ReadStatementAST, resultReg: Register) {
        val instructions = mutableListOf<Line>()
        when (node.target.type) {
            IntType -> {
                defineUtilFuncs(P_READ_INT)
                transExp(node.target, resultReg)
                addLines(
                    Move(R0, resultReg),
                    BranchLink(P_READ_INT)
                )
            }
            // complete remaining types...
        }
    }

    /**
     * Per WACC language spec, a if-block matches the grammar "if expr then stat else stat fi"
     * The if-then-else block follows the basic implementation as follows:
     *
     * [Sequence of instructions to test IF condition]
     *
     * B{cond} else
     *
     * [Sequence of instructions in THEN block]
     *
     * B fi
     *
     * else:
     *
     * [Sequence of instructions in ELSE block]
     *
     * fi: ...
     */
    @Translator(IfBlockAST::class)
    fun transIfBlock(
        stat: IfBlockAST,
        resultReg: Register
    ) {
        val curLabel = currentLabel

        // Add instructions to currentLabel
        transExp(stat.condExpr, resultReg)

        val elseLabel = newBranchLabel()
        val fiLabel = newBranchLabel()

        // compare to set condition flags and branch (can be optimised)
        addLines(
            Compare(resultReg, ImmediateOperand(0)),
            Branch(EQ, BranchLabelOperand(elseLabel))
        )

        // add sequence of instructions in THEN block under if label
        translate(stat, resultReg)

        // add branch to fi label
        addLines(
            Branch(BranchLabelOperand(fiLabel))
        )

        // add sequence of instructions in ELSE block under else label
        currentLabel = elseLabel
        translate(stat.elseBlock, resultReg)
        currentLabel = fiLabel
    }

    @Translator(NewPairAST::class)
    fun transNewPair(node: NewPairAST, resultReg: Register) {
        addLines(
            LoadWord(R0, ImmediateOperand(2 * WORD)),
            BranchLink(MALLOC),
            Move(resultReg, R0)
        )

        listOf(node.fstExpr, node.sndExpr).forEachIndexed { index, expr ->
            // Translate the expression and store it in memory with malloc
            transExp(expr, resultReg.nextReg())
            addLines(
                LoadWord(R0, ImmediateOperand(expr.type.sizeInBytes())),
                BranchLink(MALLOC)
            )

            // Store the value of the item of the pair in the address received from malloc
            addLines(
                when (expr.type.sizeInBytes()) {
                    WORD -> StoreWord(resultReg.nextReg(), ZeroOffset(R0))
                    else -> StoreByte(resultReg.nextReg(), ZeroOffset(R0))
                }
            )

            // Store the address of the pair item into the actual pairs memory
            addLines(
                StoreWord(
                    R0,
                    if (index == 0) {
                        ZeroOffset(resultReg)
                    } else {
                        ImmediateOffset(resultReg, index * WORD)
                    }
                )
            )
        }
    }

    fun transExp(expr: ExpressionAST, resultReg: Register) {
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
                val label: String = stringLabelGenerator.generate()
                data.put(label, StringData(label, expr.stringValue))
                instructions.add(LoadWord(resultReg, BranchLabelOperand(label)))
            }
            is NullPairLiteralAST -> {
//                TODO
            }
            is VariableIdentifierAST -> {
                when (expr.type) {
                    IntType -> instructions.add(
                        LoadWord(
                            SP,
                            ImmediateOffset(resultReg, state.getStackPos(expr.varName) - sp)
                        )
                    )
                    BoolType -> instructions.add(
                        LoadByte(
                            SP,
                            ImmediateOffset(resultReg, state.getStackPos(expr.varName) - sp)
                        )
                    )
                    CharType -> instructions.add(
                        LoadByte(
                            SP,
                            ImmediateOffset(resultReg, state.getStackPos(expr.varName) - sp)
                        )
                    )
                    StringType -> instructions.add(
                        LoadWord(
                            SP,
                            ImmediateOffset(resultReg, state.getStackPos(expr.varName) - sp)
                        )
                    )
                }
            }
            is ArrayElemAST -> {
//                TODO
            }
            is BinaryOpExprAST -> instructions.addAll(transBinOp(expr, resultReg))
            is UnaryOpExprAST -> instructions.addAll(transUnOp(expr, resultReg))
        }
        return instructions
    }

    fun transBinOp(expr: BinaryOpExprAST, resultReg: Register) {
        instructions.addAll(transExp(expr.expr1, resultReg))
        instructions.addAll(transExp(expr.expr2, resultReg.nextReg()))
        when {
            setOf(
                BinaryOp.PLUS,
                BinaryOp.MINUS,
                BinaryOp.MULT,
                BinaryOp.DIV,
                BinaryOp.MOD
            ).contains(expr.operator) -> {
                defineUtilFuncs(
                    P_CHECK_DIVIDE_BY_ZERO,
                    P_THROW_RUNTIME_ERROR,
                    P_PRINT_STRING
                )
                when (expr.operator) {
                    BinaryOp.MULT -> {
                        instructions.addAll(listOf(
                            Mult(
                                updateFlags = true,
                                resultReg,
                                resultReg,
                                resultReg.nextReg()
                            )
//                TODO: use SMULL and check for overflow using schema below
//                SMULL resultReg, resultReg.nextReg(), resultReg, resultReg.nextReg()
//                CMP resultReg.next(), resultReg, ASR #31
//                BLNE p_throw_overflow_error
                        ))
                    }
                    BinaryOp.DIV -> {
                        instructions.addAll(listOf(
                            Move(R0, resultReg),
                            Move(R1, resultReg.nextReg()),
                            BranchLink(P_CHECK_DIVIDE_BY_ZERO),
                            BranchLink(AEABI_IDIV),
                            Move(resultReg, R0))
                        )
                    }
                    BinaryOp.MOD -> {
                        instructions.addAll(listOf(
                            Move(R0, resultReg),
                            Move(R1, resultReg.nextReg()),
                            BranchLink(P_CHECK_DIVIDE_BY_ZERO),
                            BranchLink(AEABI_IDIVMOD),
                            Move(resultReg, R1)
                        ))
                    }
                    BinaryOp.PLUS -> {
                        instructions.addAll(listOf(
                            Add(true, resultReg, resultReg, resultReg.nextReg()),
                            BranchLink(V, P_THROW_OVERFLOW_ERROR)
                        ))
                    }
                    BinaryOp.MINUS -> {
                        instructions.addAll(listOf(
                            Sub(true, resultReg, resultReg, resultReg.nextReg()),
                            BranchLink(V, P_THROW_OVERFLOW_ERROR)
                        ))
                    }
                }
            }
            setOf(
                BinaryOp.GTE,
                BinaryOp.LT,
                BinaryOp.LTE,
                BinaryOp.EQUALS,
                BinaryOp.NOT_EQUALS
            ).contains(expr.operator) -> {
                instructions.addAll(listOf(
                    Compare(resultReg, resultReg.nextReg())
                ))
                when (expr.operator) {
                    BinaryOp.GT -> {
                        instructions.addAll(listOf(
                            Move(GT, resultReg, ImmediateOperand(1)),
                            Move(LE, resultReg, ImmediateOperand(0))
                        ))
                    }
                    BinaryOp.GTE -> {
                        instructions.addAll(listOf(
                            Move(GE, resultReg, ImmediateOperand(1)),
                            Move(LT, resultReg, ImmediateOperand(0))
                        ))
                    }
                    BinaryOp.LT -> {
                        instructions.addAll(listOf(
                            Move(LT, resultReg, ImmediateOperand(1)),
                            Move(GE, resultReg, ImmediateOperand(0))
                        ))
                    }
                    BinaryOp.LTE -> {
                        instructions.addAll(listOf(
                            Move(LE, resultReg, ImmediateOperand(1)),
                            Move(GT, resultReg, ImmediateOperand(0))
                        ))
                    }
                    BinaryOp.EQUALS -> {
                        instructions.addAll(listOf(
                            Move(EQ, resultReg, ImmediateOperand(1)),
                            Move(NE, resultReg, ImmediateOperand(0))
                        ))
                    }
                    BinaryOp.NOT_EQUALS -> {
                        instructions.addAll(listOf(
                            Move(NE, resultReg, ImmediateOperand(1)),
                            Move(EQ, resultReg, ImmediateOperand(0))
                        ))
                    }
                }
            }
            expr.operator == BinaryOp.AND -> {
                instructions.add(And(resultReg, resultReg, resultReg.nextReg()))
            }
            expr.operator == BinaryOp.OR -> {
                instructions.add(Or(resultReg, resultReg, resultReg.nextReg()))
            }
        }
        return instructions
    }

    fun transUnOp(unOpExpr: UnaryOpExprAST, resultReg: Register) {
        instructions.addAll(transExp(unOpExpr.expr, resultReg))

        when (unOpExpr.operator) {
            UnaryOp.BANG -> {
                instructions.add(Xor(resultReg, resultReg, ImmediateOperand(1)))
            }
            UnaryOp.MINUS -> {
                val label1 : String = stringLabelGenerator.generate()
                data[label1] = StringData(label1, "DivideByZeroError: divide " +
                        "or modulo by zero\\n\\0")
                val label2: String = stringLabelGenerator.generate()
                data[label2] = StringData(label2, "%.*s\\0")
                defineUtilFuncs(
                    P_CHECK_DIVIDE_BY_ZERO,
                    P_THROW_RUNTIME_ERROR,
                    P_PRINT_STRING
                )
                instructions.addAll(listOf(
                    ReverseSub(resultReg, resultReg, ImmediateOperand(0)),
                    BranchLink(P_THROW_OVERFLOW_ERROR)
                ))
            }
            UnaryOp.LEN -> {
                 instructions.add(LoadWord(resultReg, resultReg))
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

    private fun addLines(vararg lines: Instruction) {
        currentLabel.addLines(*lines)
    }

    private fun newStringLabel(str: String): StringData {
        return newStringLabel(stringLabelGenerator.generate(), str)
    }

    private fun newStringLabel(name: String, str: String): StringData {
        val label = StringData(name, str)
        data[label.name] = label
        return label
    }

    private fun newBranchLabel(vararg lines: Instruction): BranchLabel {
        return newBranchLabel(branchLabelGenerator.generate(), *lines)
    }

    private fun newBranchLabel(name: String, vararg lines: Instruction): BranchLabel {
        val label = BranchLabel(name, *lines)
        text[label.name] = label
        return label
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
