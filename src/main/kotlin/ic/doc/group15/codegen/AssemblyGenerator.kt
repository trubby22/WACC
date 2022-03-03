package ic.doc.group15.codegen

import ic.doc.group15.SymbolTable
import ic.doc.group15.WORD
import ic.doc.group15.ast.*
import ic.doc.group15.codegen.assembly.*
import ic.doc.group15.codegen.assembly.LibraryFunction.Companion.AEABI_IDIV
import ic.doc.group15.codegen.assembly.LibraryFunction.Companion.AEABI_IDIVMOD
import ic.doc.group15.codegen.assembly.LibraryFunction.Companion.EXIT
import ic.doc.group15.codegen.assembly.LibraryFunction.Companion.MALLOC
import ic.doc.group15.codegen.assembly.LibraryFunction.Companion.PUTCHAR
import ic.doc.group15.codegen.assembly.UtilFunction.*
import ic.doc.group15.codegen.assembly.instruction.*
import ic.doc.group15.codegen.assembly.instruction.ConditionCode.*
import ic.doc.group15.codegen.assembly.instruction.Directive.Companion.LTORG
import ic.doc.group15.codegen.assembly.operand.*
import ic.doc.group15.codegen.assembly.operand.Register.*
import ic.doc.group15.type.ArrayType
import ic.doc.group15.type.BasicType.*
import ic.doc.group15.type.FunctionType
import ic.doc.group15.type.PairType
import kotlin.reflect.KCallable
import kotlin.reflect.KClass

const val START_VAL = 0

private typealias TranslatorMap = Map<KClass<out ASTNode>, KCallable<*>>
private typealias TranslatorMapPair = Pair<KClass<out ASTNode>, KCallable<*>>

class AssemblyGenerator(private val ast: AST, private val st: SymbolTable) {

    private val state : State = State()

    private var sp: Int = START_VAL - 1

    private lateinit var currentLabel: BranchLabel
    private lateinit var resultRegister: Register

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

    companion object {
        private val translators: TranslatorMap by lazy {
            AssemblyGenerator::class.members.filter {
                it.annotations.isNotEmpty() && it.annotations.all { a -> a is TranslatorMethod }
            }.map {
                assert(it.annotations.size == 1)
                val annotation = it.annotations[0] as TranslatorMethod
                annotation.nodeType to it
            }.toMap()
        }
    }

    fun generate(): String {
        translate(ast)
        var asm = ""
        if (data.isNotEmpty() || utilData.isNotEmpty()) {
            asm += ".data\n\n"
        }

        // Translate the program
        transProgram(ast)

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

    private fun translate(node: ASTNode) {
        translators[node::class]?.call(this, node)
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
    @TranslatorMethod(AST::class)
    private fun transProgram(program: AST) {
        val functionASTs = program.statements.filterIsInstance<FunctionDeclarationAST>()

        // translate all function blocks into assembly
        functionASTs.forEach { f -> transFunctionDeclaration(f) }

        // Translate main instructions into assembly
        // We create a new FunctionDeclarationAST to store statements in the main function
        // and let transFunction add the entries to the text attribute

        // The symbol table contains the statements AND the FunctionDeclarationAST
        val mainAST = FunctionDeclarationAST(program, program.symbolTable, IntType, "main")
        mainAST.funcIdent = FunctionType(IntType, emptyList(), program.symbolTable)

        // Add statements
        val statementASTs = program.statements.filter { s -> s !is FunctionDeclarationAST }
        mainAST.statements.addAll(statementASTs)
        // Add return statement (main function implicitly returns 0)
        mainAST.statements.add(ReturnStatementAST(mainAST, mainAST.symbolTable.subScope(), IntLiteralAST(0)))

        transFunctionDeclaration(mainAST)
    }

    // TODO: Implement
    @TranslatorMethod(FunctionDeclarationAST::class)
    private fun transFunctionDeclaration(node: FunctionDeclarationAST) {
        // Define label
        text[currentLabel.name] = currentLabel
        val funcLabel = BranchLabel("f_$node.funcName")
        currentLabel = funcLabel

        // Translate block statements and add to loop label - we start from register R4
        // TODO: issue - interdependence of statements to be addressed
        // TODO: setup and unwind stack
        // setupStack()
        node.statements.map { s -> transStat(s, R4) }
        // unwindStack()

        // Housekeeping
        currentLabel.addLines(
            Pop(PC),
            LTORG
        )
    }

    // TODO: Implement
    @TranslatorMethod(CallAST::class)
    private fun transCall(call: CallAST) {
//        // parameters will be put onto the stack in reverse order e.g. f(a, b, c)
//        val revParams = call.actuals.toMutableList().reversed()
//        var spDec = 0
//        for (i in revParams) {
//            sp -= i.type.sizeInBytes()
//            spDec += i.type.sizeInBytes()
//            instructions.addAll(transExp(i, resultRegister))
//            instructions.add(Sub(SP, SP, ImmediateOperand(-i.type.sizeInBytes())))
//            instructions.add(StoreWord(SP, ZeroOffset(resultRegister)))
//        }
//        instructions.add(BranchLink("p_" + call.funcName))
//        instructions.add(Add(SP, SP, ImmediateOperand(spDec)))
//        instructions.add(Move(resultRegister, R0))
    }

    @TranslatorMethod(VariableDeclarationAST::class)
    private fun transVariableDeclaration(node: VariableDeclarationAST) {
        val instructions = mutableListOf<Line>()
        sp -= node.varIdent.type.sizeInBytes()
        state.setStackPos(node.varName, sp)
        translate(node.rhs, resultRegister)
        addLines(
            StoreWord(SP, ImmediateOffset(resultRegister, state.getStackPos(node.varName)))
        )
    }

    @TranslatorMethod(AssignToIdentAST::class)
    private fun transAssignToIdent(node: AssignToIdentAST) {
        translate(node.rhs, resultRegister)
        addLines(
            StoreWord(
                SP,
                ImmediateOffset(resultRegister, state.getStackPos(node.lhs.varName) - sp)
            )
        )
    }

    // TOOD: Implement
    @TranslatorMethod(AssignToArrayElemAST::class)
    private fun transAssignToArrayElem(node: AssignToArrayElemAST) {

    }

    // TODO: Implement
    @TranslatorMethod(AssignToPairElemAST::class)
    private fun transAssignToPairElem(node: AssignToPairElemAST) {

    }

    fun transAssignToArrayElem(
        node: AssignToArrayElemAST, resultReg:
        Register
    ): List<Line> {
        val instructions: MutableList<Line> = mutableListOf()
        instructions.addAll(transAssignRhs(node.rhs, resultReg))
        val arrayElemAST = node.lhs as ArrayElemAST
        val symbolTable = arrayElemAST.symbolTable
        val arrayName = arrayElemAST.arrayName
//        TODO: calculate stack pointer offset of array pointer using values
//         above
        val stackPointerOffset = 0
        instructions.addAll(listOf(
            Add(resultReg.nextReg(), SP, ImmediateOperand(stackPointerOffset)),
//            Assumption: array is 1-dimensional
            LoadWord(resultReg.nextReg().nextReg(), ImmediateOperand
                (arrayElemAST.indexExpr[0]))
        ))
        return instructions
    }

    @TranslatorMethod(FstPairElemAST::class)
    fun transFstPairElem(node: FstPairElemAST) {

    }

    @TranslatorMethod(SndPairElemAST::class)
    fun transSndPairElem(node: SndPairElemAST) {

    }

    /**
     * Per WACC language spec, a while-block matches the grammar "while expr do stat done"
     * The while block follows the basic implementation as follows:
     *
     * B check
     *
     * loop:
     *
     * {Sequence of instructions in WHILE block}
     *
     * check:
     *
     * {Sequence of instructions to test WHILE condition}
     *
     * CMP resultReg, 1
     * BEQ loop
     */
    @TranslatorMethod(WhileBlockAST::class)
    private fun transWhileBlock(node: WhileBlockAST) {
        // Define labels
        val loopLabel = BranchLabel(branchLabelGenerator.generate())
        val checkLabel = BranchLabel(branchLabelGenerator.generate())

        // Add branch instruction
        addLines(
            Branch(BranchLabelOperand(checkLabel))
        )
        text[currentLabel.name] = currentLabel

        // Translate block statements and add to loop label
        // TODO: issue - interdependence of statements to be addressed
        currentLabel = loopLabel
        node.statements.forEach { s -> translate(s, resultRegister) }
        // TODO: the statements should have to setup stack and unwind stack
        text[currentLabel.name] = currentLabel

        // Translate condition statements and add to check label
        currentLabel = checkLabel
        translate(node.condExpr, resultRegister)

        // Add compare and branch instruction
        currentLabel.addLines(
            Compare(resultRegister, ImmediateOperand(1)),
            Branch(EQ, BranchLabelOperand(loopLabel))
        )
    }

    @TranslatorMethod(BeginEndBlockAST::class)
    fun transBeginEndBlock(node: BeginEndBlockAST) {
        node.statements.forEach { translate(it) }
    }

    @TranslatorMethod(ArrayElemAST::class)
    fun transArrayElem(node: ArrayElemAST) {

    }

    @TranslatorMethod(FreeStatementAST::class)
    fun transFreeStatement(node: FreeStatementAST) {

    }

    @TranslatorMethod(ReturnStatementAST::class)
    fun transReturnStatement(node: ReturnStatementAST) {

    }

    @TranslatorMethod(ExitStatementAST::class)
    fun transExitStatement(node: ExitStatementAST) {

    }

    /**
     * Per WACC language spec, a return statement has the format "return x". It can
     * only exist in a body of a non-main function and is used to return a value from
     * that function.
     */
    fun transReturnStatement(node: ReturnStatementAST, resultReg: Register) {
        translate(node.expr, resultReg)
        addLines(
            Move(R0, resultReg),
            Pop(PC)
        )
    }

    /**
     * Per WACC language spec, exit statements have the format "exit x", where x is
     * an exit code of type int in range [0, 255].
     */
    fun transExitStatement(node: ExitStatementAST, resultReg: Register) {
        translate(node.expr, resultReg)
        addLines(
            Move(R0, resultReg),
            BranchLink(EXIT)
        )
    }

    @TranslatorMethod(PrintStatementAST::class)
    fun transPrintStatement(node: PrintStatementAST) {
        translate(node.expr, resultRegister)
        addLines(
            Move(R0, resultRegister)
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

    @TranslatorMethod(PrintlnStatementAST::class)
    fun transPrintlnStatement(node: PrintlnStatementAST, resultRegister: Register) {
        val printStatementAST = PrintStatementAST(node.parent!!, node.symbolTable, node.expr)
        transPrintStatement(printStatementAST, resultRegister)
        addLines(BranchLink(P_PRINT_LN))
    }

    /**
     * Per WACC language spec, a read statement follows the grammar "read x", where x
     * is either of character or integer type.
     *
     * If the input is of type int, then it will convert the input string into an integer
     * (truncated to fit within the minimum/maximum range of the int type). If the standard
     * input value is incompatible with the type of the target, then this value will not be
     * consumed from the standard input stream. Instead, the program will continue, leaving
     * the target's value unchanged.
     */
    @TranslatorMethod(ReadStatementAST::class)
    fun transReadStatement(node: ReadStatementAST) {
        when (node.target.type) {
            IntType -> {
                defineUtilFuncs(P_READ_INT)
                translate(node.target, resultRegister)
                addLines(
                    Move(R0, resultRegister),
                    BranchLink(P_READ_INT)
                )
            }
            CharType -> {
                defineUtilFuncs(P_READ_CHAR)
                translate(node.target, resultRegister)
                addLines(
                    Move(R0, resultRegister),
                    BranchLink(P_READ_CHAR)
                )
            }
            else -> throw IllegalArgumentException("Read does not support input of type ${node.target.type}")
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
    @TranslatorMethod(IfBlockAST::class)
    fun transIfBlock(
        stat: IfBlockAST,
        resultRegister: Register
    ) {
        val curLabel = currentLabel

        // Add instructions to currentLabel
        translate(stat.condExpr, resultRegister)

        val elseLabel = newBranchLabel()
        val fiLabel = newBranchLabel()

        // compare to set condition flags and branch (can be optimised)
        addLines(
            Compare(resultRegister, ImmediateOperand(0)),
            Branch(EQ, BranchLabelOperand(elseLabel))
        )

        // add sequence of instructions in THEN block under if label
        translate(stat, resultRegister)

        // add branch to fi label
        addLines(
            Branch(BranchLabelOperand(fiLabel))
        )

        // add sequence of instructions in ELSE block under else label
        currentLabel = elseLabel
        stat.elseBlock.statements.forEach { translate(it) }
        currentLabel = fiLabel
    }

    @TranslatorMethod(NewPairAST::class)
    fun transNewPair(node: NewPairAST) {
        addLines(
            LoadWord(R0, ImmediateOperand(2 * WORD)),
            BranchLink(MALLOC),
            Move(resultRegister, R0)
        )

        listOf(node.fstExpr, node.sndExpr).forEachIndexed { index, expr ->
            // Translate the expression and store it in memory with malloc
            resultRegister = resultRegister.nextReg()
            translate(expr)
            addLines(
                LoadWord(R0, ImmediateOperand(expr.type.sizeInBytes())),
                BranchLink(MALLOC)
            )

            // Store the value of the item of the pair in the address received from malloc
            addLines(
                when (expr.type.sizeInBytes()) {
                    WORD -> StoreWord(resultRegister.nextReg(), ZeroOffset(R0))
                    else -> StoreByte(resultRegister.nextReg(), ZeroOffset(R0))
                }
            )

            // Store the address of the pair item into the actual pairs memory
            addLines(
                StoreWord(
                    R0,
                    if (index == 0) {
                        ZeroOffset(resultRegister)
                    } else {
                        ImmediateOffset(resultRegister, index * WORD)
                    }
                )
            )
        }
    }

    //endregion

    //region translateExpr

    @TranslatorMethod(IntLiteralAST::class)
    private fun translateIntLiteral(node: IntLiteralAST) {
        addLines(
            LoadWord(resultRegister, ImmediateOperand(node.intValue))
        )
    }

    @TranslatorMethod(BoolLiteralAST::class)
    private fun translateBoolLiteral(node: BoolLiteralAST) {
        addLines(
            Move(resultRegister, ImmediateOperand(node.boolValue))
        )
    }

    @TranslatorMethod(CharLiteralAST::class)
    private fun translateCharLiteral(node: CharLiteralAST) {
        addLines(
            Move(resultRegister, ImmediateOperand(node.charValue))
        )
    }

    @TranslatorMethod(StringLiteralAST::class)
    private fun translateStringLiteral(node: StringLiteralAST) {
        addLines(
            LoadWord(resultRegister, DataLabelOperand(newStringLabel(node.stringValue)))
        )
    }

    @TranslatorMethod(NullPairLiteralAST::class)
    private fun translateNullPairLiteral(node: NullPairLiteralAST) {
        // TODO
    }

    @TranslatorMethod(ArrayLiteralAST::class)
    private fun transArrayLiteral(node: ArrayLiteralAST) {
        val size = 4 + (node.elems.size * (node.elems[0].type.sizeInBytes())) // calculate bytes need to malloc
        currentLabel.addLines(
            LoadWord(R0, ImmediateOperand(size)),
            BranchLink(MALLOC),
            Move(resultRegister, R0)
        )
        var offset = 0 // now we go in this for loop to put all the items of the array into the memory of the array
        for (expr in node.elems) {
            resultRegister = resultRegister.nextReg()
            translate(expr)
            if (expr.type.sizeInBytes() == 4) {
                offset += 4
                currentLabel.addLines(
                    StoreWord(resultRegister.nextReg(), ImmediateOffset(resultRegister, offset))
                )
            } else {
                offset += 1
                currentLabel.addLines(
                    StoreByte(resultRegister.nextReg(), ImmediateOffset(resultRegister, offset))
                )
            }
        }
        currentLabel.addLines(
            LoadWord(resultRegister.nextReg(), ImmediateOperand(node.elems.size)),
            StoreWord(resultRegister.nextReg(), ZeroOffset(resultRegister))
        )
    }

    @TranslatorMethod(VariableIdentifierAST::class)
    private fun translateVariableIdentifier(node: VariableIdentifierAST) {
        addLines(
            LoadWord(
                SP,
                ImmediateOffset(resultRegister, state.getStackPos(node.varName) - sp)
            )
        )
    }

    @TranslatorMethod(ArrayElemAST::class)
    private fun translateArrayElem(node: ArrayElemAST) {
        // TODO
    }

    @TranslatorMethod(BinaryOpExprAST::class)
    private fun transBinOp(expr: BinaryOpExprAST) {
        instructions.addAll(transExp(expr.expr1, resultRegister))
        instructions.addAll(transExp(expr.expr2, resultRegister.nextReg()))
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
                                resultRegister,
                                resultRegister,
                                resultRegister.nextReg()
                            )
//                TODO: use SMULL and check for overflow using schema below
//                SMULL resultRegister, resultRegister.nextReg(), resultRegister, resultRegister.nextReg()
//                CMP resultRegister.next(), resultRegister, ASR #31
//                BLNE p_throw_overflow_error
                        ))
                    }
                    BinaryOp.DIV -> {
                        instructions.addAll(listOf(
                            Move(R0, resultRegister),
                            Move(R1, resultRegister.nextReg()),
                            BranchLink(P_CHECK_DIVIDE_BY_ZERO),
                            BranchLink(AEABI_IDIV),
                            Move(resultRegister, R0))
                        )
                    }
                    BinaryOp.MOD -> {
                        instructions.addAll(listOf(
                            Move(R0, resultRegister),
                            Move(R1, resultRegister.nextReg()),
                            BranchLink(P_CHECK_DIVIDE_BY_ZERO),
                            BranchLink(AEABI_IDIVMOD),
                            Move(resultRegister, R1)
                        ))
                    }
                    BinaryOp.PLUS -> {
                        instructions.addAll(listOf(
                            Add(true, resultRegister, resultRegister, resultRegister.nextReg()),
                            BranchLink(V, P_THROW_OVERFLOW_ERROR)
                        ))
                    }
                    BinaryOp.MINUS -> {
                        instructions.addAll(listOf(
                            Sub(true, resultRegister, resultRegister, resultRegister.nextReg()),
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
                    Compare(resultRegister, resultRegister.nextReg())
                ))
                when (expr.operator) {
                    BinaryOp.GT -> {
                        instructions.addAll(listOf(
                            Move(GT, resultRegister, ImmediateOperand(1)),
                            Move(LE, resultRegister, ImmediateOperand(0))
                        ))
                    }
                    BinaryOp.GTE -> {
                        instructions.addAll(listOf(
                            Move(GE, resultRegister, ImmediateOperand(1)),
                            Move(LT, resultRegister, ImmediateOperand(0))
                        ))
                    }
                    BinaryOp.LT -> {
                        instructions.addAll(listOf(
                            Move(LT, resultRegister, ImmediateOperand(1)),
                            Move(GE, resultRegister, ImmediateOperand(0))
                        ))
                    }
                    BinaryOp.LTE -> {
                        instructions.addAll(listOf(
                            Move(LE, resultRegister, ImmediateOperand(1)),
                            Move(GT, resultRegister, ImmediateOperand(0))
                        ))
                    }
                    BinaryOp.EQUALS -> {
                        instructions.addAll(listOf(
                            Move(EQ, resultRegister, ImmediateOperand(1)),
                            Move(NE, resultRegister, ImmediateOperand(0))
                        ))
                    }
                    BinaryOp.NOT_EQUALS -> {
                        instructions.addAll(listOf(
                            Move(NE, resultRegister, ImmediateOperand(1)),
                            Move(EQ, resultRegister, ImmediateOperand(0))
                        ))
                    }
                }
            }
            expr.operator == BinaryOp.AND -> {
                instructions.add(And(resultRegister, resultRegister, resultRegister.nextReg()))
            }
            expr.operator == BinaryOp.OR -> {
                instructions.add(Or(resultRegister, resultRegister, resultRegister.nextReg()))
            }
        }
        return instructions
    }

    @TranslatorMethod(UnaryOpExprAST::class)
    private fun transUnOp(unOpExpr: UnaryOpExprAST) {
        instructions.addAll(transExp(unOpExpr.expr, resultRegister))

        when (unOpExpr.operator) {
            UnaryOp.BANG -> {
                instructions.add(Xor(resultRegister, resultRegister, ImmediateOperand(1)))
            }
            UnaryOp.MINUS -> {
                defineUtilFuncs(
                    P_CHECK_DIVIDE_BY_ZERO,
                    P_THROW_RUNTIME_ERROR,
                    P_PRINT_STRING
                )
                instructions.addAll(listOf(
                    ReverseSub(resultRegister, resultRegister, ImmediateOperand(0)),
                    BranchLink(P_THROW_OVERFLOW_ERROR)
                ))
            }
            UnaryOp.LEN -> {
                 instructions.add(LoadWord(resultRegister, resultRegister))
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

    //endregion

    //region helpers

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

    private fun evaluateIntExpr(expr: ExpressionAST): Int {
        assert(expr.type == IntType)
        when (expr) {
            is IntLiteralAST -> {
                return expr.intValue
            }
            is VariableIdentifierAST -> {

            }
            is ArrayElemAST -> {

            }
            is UnaryOpExprAST -> {
                when (expr.operator) {
                    UnaryOp.MINUS -> return - evaluateIntExpr(expr.expr)
                    UnaryOp.LEN -> {
                        val arrayIdentifier = expr.expr as
                        VariableIdentifierAST
//                        TODO
                    }
                    UnaryOp.ORD -> return evaluateCharExpr(expr.expr).code
                }
            }
            is BinaryOpExprAST -> {

            }
        }
        throw Error("Should not get here")
    }

    private fun evaluateBoolExpr(expr: ExpressionAST): Boolean {
        assert(expr.type == BoolType)
        return false
    }

    private fun evaluateCharExpr(expr: ExpressionAST): Char {
        assert(expr.type == CharType)
        when (expr) {
            is CharLiteralAST -> return expr.charValue
            is UnaryOpExprAST -> return evaluateIntExpr(expr.expr).toChar()
            is ArrayElemAST -> {
//                TODO
            }
        }
        throw Error("Should not get here")
    }

    private fun evaluateStringExpr(expr: ExpressionAST): String {
        assert(expr.type == StringType)
        when (expr) {
            is StringLiteralAST -> return expr.stringValue
            is ArrayElemAST -> {
//                TODO
            }
            is PairElemAST -> {
//                TODO
            }
        }
        throw Error("Should not get here")
    }
    //endregion
}
