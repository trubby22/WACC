package ic.doc.group15.codegen

import ic.doc.group15.BYTE
import ic.doc.group15.SymbolTable
import ic.doc.group15.WORD
import ic.doc.group15.ast.*
import ic.doc.group15.ast.BinaryOp.*
import ic.doc.group15.codegen.assembly.* // ktlint-disable no-unused-imports
import ic.doc.group15.codegen.assembly.LibraryFunction.Companion.AEABI_IDIV
import ic.doc.group15.codegen.assembly.LibraryFunction.Companion.AEABI_IDIVMOD
import ic.doc.group15.codegen.assembly.LibraryFunction.Companion.EXIT
import ic.doc.group15.codegen.assembly.LibraryFunction.Companion.MALLOC
import ic.doc.group15.codegen.assembly.LibraryFunction.Companion.PUTCHAR
import ic.doc.group15.codegen.assembly.UtilFunction.*
import ic.doc.group15.codegen.assembly.instruction.*
import ic.doc.group15.codegen.assembly.instruction.ConditionCode.*
import ic.doc.group15.codegen.assembly.instruction.ConditionCode.GT
import ic.doc.group15.codegen.assembly.instruction.ConditionCode.LT
import ic.doc.group15.codegen.assembly.instruction.Directive.Companion.LTORG
import ic.doc.group15.codegen.assembly.operand.*
import ic.doc.group15.codegen.assembly.operand.Register.*
import ic.doc.group15.type.ArrayType
import ic.doc.group15.type.BasicType.*
import ic.doc.group15.type.FunctionType
import ic.doc.group15.type.PairType
import ic.doc.group15.type.Variable
import kotlin.reflect.KCallable
import kotlin.reflect.KClass

const val START_VAL = 0

private typealias TranslatorMap = Map<KClass<out ASTNode>, KCallable<*>>
private typealias TranslatorMapPair = Pair<KClass<out ASTNode>, KCallable<*>>

class AssemblyGenerator(private val ast: AST, private val st: SymbolTable) {

    private val state: State = State()

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
     * Sets up the environment for the block of statements, specifically by initialising the stack variables
     */
    private fun blockPrologue(node: BlockAST) {
        // Calculate how much space to be allocated (and modify each variable to include its position on the stack)
        var currentStackPosition = node.symbolTable.getStackSize()

        // Setup stack
        addLines(Sub(SP, SP, ImmediateOperand(currentStackPosition)))

        // Calculate the stack position for each variable
        val variables = node.symbolTable.getMap().keys.filterIsInstance<Variable>()
        for (v in variables) {
            currentStackPosition -= v.type.sizeInBytes()
            v.stackPosition = currentStackPosition
        }
    }

    /**
     * Restores the state so that the program can resume from where it left off before entering the block
    */
    private fun blockEpilogue(node: BlockAST) {
        // Unwind stack
        val stackSpaceUsed = node.symbolTable.getStackSize()
        addLines(Add(SP, SP, ImmediateOperand(stackSpaceUsed)))
    }

    /**
     * Sets up the environment for a function
     */
    private fun functionPrologue(node: FunctionDeclarationAST) {
        addLines(Push(LR))
    }

    /**
     * Restores the state so that the program can resume from where it left off before entering the function
     */
    private fun functionEpilogue(node: BlockAST) {
        addLines(
            Pop(PC),
            LTORG
        )
    }

    /**
     * Sets up the stack with the necessary parameters before a function call.
     */
    private fun functionCallPrologue(node: FunctionDeclarationAST) {
        node.formals.forEach {
            // Load variable to resultRegister
            translate(it)
            addLines(
                // Allocate space in stack
                Sub(SP, SP, ImmediateOperand(it.type.sizeInBytes())),
                // Move value from resultRegister to stack
                StoreWord(resultRegister, ZeroOffset(SP))
            )

        }
    }

    /**
     * Restores the state so that the program can resume from where it left off before the
     * function sub-call was made
     */
    private fun functionCallEpilogue(node: FunctionDeclarationAST) {
        // Increment the stack pointer back to where it was
        val stackSpaceUsed = node.formals.sumOf { arg -> arg.type.sizeInBytes() }

        addLines(Add(SP, SP, ImmediateOperand(stackSpaceUsed)))
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
        resultRegister = R4
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
        node.statements.map { s -> translate(s) }
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
        sp -= node.varIdent.type.sizeInBytes()
        state.setStackPos(node.varName, sp)
        translate(node.rhs)
        addLines(
            StoreWord(SP, ImmediateOffset(resultRegister, state.getStackPos(node.varName)))
        )
    }

    @TranslatorMethod(AssignToIdentAST::class)
    private fun transAssignToIdent(node: AssignToIdentAST) {
        translate(node.rhs)
        addLines(
            StoreWord(
                SP,
                ImmediateOffset(resultRegister, state.getStackPos(node.lhs.varName) - sp)
            )
        )
    }

    @TranslatorMethod(AssignToPairElemAST::class)
    private fun transAssignToPairElem(node: AssignToPairElemAST) {
        translate(node.rhs)
//        TODO: calculate pairStackOffset instead of assuming it's 0
        val pairStackOffset = 0
        val storeInstruction = when (node.rhs.type.sizeInBytes()) {
            1 -> StoreByte(resultRegister, ZeroOffset(resultRegister.nextReg()))
            else -> StoreWord(resultRegister, ZeroOffset(resultRegister.nextReg()))
        }
        addLines(
            LoadWord(resultRegister.nextReg(), ImmediateOffset(SP, pairStackOffset)),
            Move(R0, resultRegister.nextReg()),
            BranchLink(P_CHECK_NULL_POINTER),
            LoadWord(
                resultRegister.nextReg(),
                ZeroOffset(
                    resultRegister
                        .nextReg()
                )
            ),
            storeInstruction
        )
    }

    @TranslatorMethod(AssignToArrayElemAST::class)
    fun transAssignToArrayElem(node: AssignToArrayElemAST) {
        translate(node.rhs)
        val arrayElemAST = node.lhs as ArrayElemAST
        val symbolTable = arrayElemAST.symbolTable
        val arrayName = arrayElemAST.arrayName
//        TODO: calculate stack pointer offset of array pointer using values
//         above and calculate hardcodedNum's instead of
//         hardcoding them
        val stackPointerOffset = 0
        val hardcodedNum1 = 4
        val hardcodedNum2 = 2
        addLines(Add(resultRegister.nextReg(), SP, ImmediateOperand(stackPointerOffset)))
        for (indexExpr in arrayElemAST.indexExpr) {
            addLines(translate(indexExpr) as List<Instruction>)
            addLines(
                LoadWord(
                    resultRegister.nextReg().nextReg(),
                    resultRegister.nextReg().nextReg()
                ),
                LoadWord(
                    resultRegister.nextReg(),
                    ZeroOffset(resultRegister.nextReg())
                ),
                Move(R0, resultRegister.nextReg().nextReg()),
                Move(R1, resultRegister.nextReg().nextReg()),
                BranchLink(P_CHECK_ARRAY_BOUNDS),
                Add(
                    resultRegister.nextReg(),
                    resultRegister.nextReg(),
                    ImmediateOperand
                    (hardcodedNum1)
                ),
                Add(
                    resultRegister.nextReg(),
                    resultRegister.nextReg(),
//                        Sometimes LSL is performed, sometimes not - I don't
//                        know what it depends on
//                        TODO: perform LSL only when needed
                    LogicalShiftLeft
                    (resultRegister.nextReg().nextReg(), hardcodedNum2)
                )
            )
        }
        addLines(
            StoreWord(resultRegister, ZeroOffset(resultRegister.nextReg()))
        )
    }

    @TranslatorMethod(FstPairElemAST::class)
    fun transFstPairElem(node: FstPairElemAST) {
//        TODO: calculate offset
        val pairPointerOffset = 0
        val loadInstruction = if (node.pairExpr.type.sizeInBytes() == BYTE) {
            LoadByte(resultRegister, ZeroOffset
                (resultRegister))
        } else {
            LoadWord(resultRegister, ZeroOffset
                (resultRegister))
        }
        addLines(
            LoadWord(resultRegister, ImmediateOffset(SP, pairPointerOffset)),
            Move(R0, resultRegister),
            BranchLink(P_CHECK_NULL_POINTER),
            LoadWord(resultRegister, ZeroOffset(resultRegister)),
            loadInstruction
        )
    }

    @TranslatorMethod(SndPairElemAST::class)
    fun transSndPairElem(node: SndPairElemAST) {
//        TODO: calculate offset
        val pairPointerOffset = 0
        val sizeOfFstElem = (node.pairExpr.type as PairType).sndType.sizeInBytes()
        val loadInstruction = if (node.pairExpr.type.sizeInBytes() == BYTE) {
            LoadByte(resultRegister, ZeroOffset
                (resultRegister))
        } else {
            LoadWord(resultRegister, ZeroOffset
                (resultRegister))
        }
        addLines(
            LoadWord(resultRegister, ImmediateOffset(SP, pairPointerOffset)),
            Move(R0, resultRegister),
            BranchLink(P_CHECK_NULL_POINTER),
            LoadWord(resultRegister, ImmediateOffset(resultRegister, sizeOfFstElem)),
            loadInstruction
        )
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
        node.statements.forEach { translate(it) }

        // Translate condition statements and add to check label
        currentLabel = checkLabel
        translate(node.condExpr)

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

    @TranslatorMethod(FreeStatementAST::class)
    fun transFreeStatement(node: FreeStatementAST) {
//        TODO
    }

    /**
     * return statements have the format "return x". It can
     * only exist in a body of a non-main function and is used to return a value from
     * that function.
     */
    @TranslatorMethod(ReturnStatementAST::class)
    fun transReturnStatement(node: ReturnStatementAST) {
        translate(node.expr)
        addLines(
            Move(R0, resultRegister),
            Pop(PC)
        )
    }

    /**
     * Per WACC language spec, exit statements have the format "exit x", where x is
     * an exit code of type int in range [0, 255].
     */
    @TranslatorMethod(ExitStatementAST::class)
    fun transExitStatement(node: ExitStatementAST) {
        translate(node.expr)
        addLines(
            Move(R0, resultRegister),
            BranchLink(EXIT)
        )
    }

    @TranslatorMethod(PrintStatementAST::class)
    fun transPrintStatement(node: PrintStatementAST) {
        translate(node.expr)
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
        transPrintStatement(printStatementAST)
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
                translate(node.target)
                addLines(
                    Move(R0, resultRegister),
                    BranchLink(P_READ_INT)
                )
            }
            CharType -> {
                defineUtilFuncs(P_READ_CHAR)
                translate(node.target)
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
        translate(stat.condExpr)

        val elseLabel = newBranchLabel()
        val fiLabel = newBranchLabel()

        // compare to set condition flags and branch (can be optimised)
        addLines(
            Compare(resultRegister, ImmediateOperand(0)),
            Branch(EQ, BranchLabelOperand(elseLabel))
        )

        // add sequence of instructions in THEN block under if label
        translate(stat)

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
//        TODO: Calculate SP offset instead of assuming it's 0
        val stackPointerOffset = 0
        addLines(
            LoadWord(resultRegister, ImmediateOffset(SP, stackPointerOffset))
        )
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
        addLines(LoadWord(resultRegister, ImmediateOffset(SP, state.getStackPos(node.arrayName) - sp)))
        for (i in node.indexExpr.indices) {
            val index = node.indexExpr.get(i)
            translate(index) // translating the index of the array
            addLines(Move(R0, resultRegister.nextReg())) // this and next two lines just check bounds of array
            addLines(Move(R1, resultRegister)) //
            addLines(BranchLink(P_CHECK_ARRAY_BOUNDS)) //
            addLines(Add(resultRegister, resultRegister, ImmediateOperand(4))) // move past array size
            if (i == node.indexExpr.lastIndex) {
                if (node.elemType.sizeInBytes() == 4) {
                    addLines(Add(resultRegister, resultRegister, LogicalShiftLeft(resultRegister.nextReg(), 2))) // get address of desired index into result reg
                    addLines(LoadWord(resultRegister, resultRegister)) // put whats at that index into result reg
                } else if (node.elemType.sizeInBytes() == 1) {
                    addLines(Add(resultRegister, resultRegister, resultRegister.nextReg()))
                    addLines(LoadByte(resultRegister, ZeroOffset(resultRegister)))
                }
            } else {
                addLines(Add(resultRegister, resultRegister, LogicalShiftLeft(resultRegister.nextReg(), 2))) // get address of desired index into result reg
                addLines(LoadWord(resultRegister, resultRegister)) // put whats at that index into result reg
            }
        }
    }

    @TranslatorMethod(BinaryOpExprAST::class)
    private fun transBinOp(expr: BinaryOpExprAST) {
        translate(expr.expr1)
        resultRegister = resultRegister.nextReg()
        translate(expr.expr2)
        when (expr.operator) {
            PLUS, MINUS, MULT, DIV, MOD -> {
                defineUtilFuncs(
                    P_CHECK_DIVIDE_BY_ZERO,
                    P_THROW_RUNTIME_ERROR,
                    P_PRINT_STRING
                )
                when (expr.operator) {
                    MULT -> {
                        addLines(
                            Mult(
                                updateFlags = true,
                                resultRegister,
                                resultRegister,
                                resultRegister.nextReg()
                            ),
//                            TODO: use SMULL using schema below
//                             SMULL resultRegister, resultRegister.nextReg(), resultRegister, resultRegister.nextReg()
                            Compare(resultRegister.nextReg(),
                                ArithmeticShiftRight(resultRegister, 31)),
                            BranchLink(NE, P_THROW_OVERFLOW_ERROR)
                        )
                    }
                    DIV -> {
                        addLines(
                            Move(R0, resultRegister),
                            Move(R1, resultRegister.nextReg()),
                            BranchLink(P_CHECK_DIVIDE_BY_ZERO),
                            BranchLink(AEABI_IDIV),
                            Move(resultRegister, R0)
                        )
                    }
                    MOD -> {
                        addLines(
                            Move(R0, resultRegister),
                            Move(R1, resultRegister.nextReg()),
                            BranchLink(P_CHECK_DIVIDE_BY_ZERO),
                            BranchLink(AEABI_IDIVMOD),
                            Move(resultRegister, R1)
                        )
                    }
                    PLUS -> {
                        addLines(
                            Add(true, resultRegister, resultRegister, resultRegister.nextReg()),
                            BranchLink(V, P_THROW_OVERFLOW_ERROR)
                        )
                    }
                    MINUS -> {
                        addLines(
                            Sub(true, resultRegister, resultRegister, resultRegister.nextReg()),
                            BranchLink(V, P_THROW_OVERFLOW_ERROR)
                        )
                    }
                }
            }
            BinaryOp.GT, GTE, BinaryOp.LT, LTE, EQUALS, NOT_EQUALS -> {
                addLines(
                    Compare(resultRegister, resultRegister.nextReg())
                )
                when (expr.operator) {
                    BinaryOp.GT -> {
                        addLines(
                            Move(GT, resultRegister, ImmediateOperand(1)),
                            Move(LE, resultRegister, ImmediateOperand(0))
                        )
                    }
                    GTE -> {
                        addLines(
                            Move(GE, resultRegister, ImmediateOperand(1)),
                            Move(LT, resultRegister, ImmediateOperand(0))
                        )
                    }
                    BinaryOp.LT -> {
                        addLines(
                            Move(LT, resultRegister, ImmediateOperand(1)),
                            Move(GE, resultRegister, ImmediateOperand(0))
                        )
                    }
                    LTE -> {
                        addLines(
                            Move(LE, resultRegister, ImmediateOperand(1)),
                            Move(GT, resultRegister, ImmediateOperand(0))
                        )
                    }
                    EQUALS -> {
                        addLines(
                            Move(EQ, resultRegister, ImmediateOperand(1)),
                            Move(NE, resultRegister, ImmediateOperand(0))
                        )
                    }
                    NOT_EQUALS -> {
                        addLines(
                            Move(NE, resultRegister, ImmediateOperand(1)),
                            Move(EQ, resultRegister, ImmediateOperand(0))
                        )
                    }
                }
            }
            AND -> {
                addLines(And(resultRegister, resultRegister, resultRegister.nextReg()))
            }
            OR -> {
                addLines(Or(resultRegister, resultRegister, resultRegister.nextReg()))
            }
        }
    }

    @TranslatorMethod(UnaryOpExprAST::class)
    private fun transUnOp(unOpExpr: UnaryOpExprAST) {
        translate(unOpExpr.expr)

        when (unOpExpr.operator) {
            UnaryOp.BANG -> {
                addLines(
                    Xor(resultRegister, resultRegister, ImmediateOperand(1))
                )
            }
            UnaryOp.MINUS -> {
                defineUtilFuncs(
                    P_CHECK_DIVIDE_BY_ZERO,
                    P_THROW_RUNTIME_ERROR,
                    P_PRINT_STRING
                )
                addLines(
                    ReverseSub(resultRegister, resultRegister, ImmediateOperand(0)),
                    BranchLink(P_THROW_OVERFLOW_ERROR)
                )
            }
            UnaryOp.LEN -> {
                addLines(
                    LoadWord(resultRegister, resultRegister)
                )
            }
            UnaryOp.ORD -> {
//                No actions needed since int ~ char
            }
            UnaryOp.CHR -> {
//                No actions needed since int ~ char
            }
        }
    }

    //endregion

    //region helpers

    private fun addLines(vararg lines: Instruction) {
        currentLabel.addLines(*lines)
    }

    private fun addLines(lines: Collection<Instruction>) {
        currentLabel.addLines(lines)
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

    //endregion
}
