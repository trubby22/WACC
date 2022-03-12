package ic.doc.group15.visitor

import ic.doc.group15.SymbolTable
import ic.doc.group15.assembly.*
import ic.doc.group15.assembly.LibraryFunction.Companion.AEABI_IDIV
import ic.doc.group15.assembly.LibraryFunction.Companion.AEABI_IDIVMOD
import ic.doc.group15.assembly.LibraryFunction.Companion.EXIT
import ic.doc.group15.assembly.LibraryFunction.Companion.MALLOC
import ic.doc.group15.assembly.LibraryFunction.Companion.PUTCHAR
import ic.doc.group15.assembly.UtilFunction.*
import ic.doc.group15.assembly.instruction.*
import ic.doc.group15.assembly.instruction.ConditionCode.*
import ic.doc.group15.assembly.instruction.ConditionCode.GT
import ic.doc.group15.assembly.instruction.ConditionCode.LT
import ic.doc.group15.assembly.instruction.Directive.Companion.LTORG
import ic.doc.group15.assembly.operand.*
import ic.doc.group15.assembly.operand.Register.*
import ic.doc.group15.ast.*
import ic.doc.group15.ast.BinaryOp.*
import ic.doc.group15.type.ArrayType
import ic.doc.group15.type.BasicType.*
import ic.doc.group15.type.PairType
import ic.doc.group15.type.Param
import ic.doc.group15.type.Variable
import ic.doc.group15.util.BYTE
import ic.doc.group15.util.WORD
import java.lang.IllegalArgumentException
import java.util.*
import kotlin.reflect.KCallable
import kotlin.reflect.KClass
import kotlin.reflect.jvm.isAccessible

private typealias TranslatorMap = Map<KClass<out ASTNode>, KCallable<*>>

class AssemblyGenerator(
    private val ast: AST,
    private val enableLogging: Boolean = true
) {
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

    /**
     * Store the total stack space used to store intermediate results of
     * the current scope. Since we may encounter nested scopes, we want to
     * store the offset when entering a sub-scope and restore it when we exit
     * the sub-scope.
     */
    private val offsetStackStore = LinkedList<Int>()

    private val stringLabelGenerator = UniqueStringLabelGenerator()
    private val branchLabelGenerator = UniqueBranchLabelGenerator()

    companion object {

        private const val MAX_STACK_CHANGE = 1024

        private val translators: TranslatorMap by lazy {
            AssemblyGenerator::class.members.filter {
                it.annotations.isNotEmpty() && it.annotations.all { a -> a is TranslatorMethod }
            }.map {
                assert(it.annotations.size == 1)
                it.isAccessible = true
                val annotation = it.annotations[0] as TranslatorMethod
                annotation.nodeType to it
            }.toMap()
        }
    }

    fun generate(): String {
        log("Translating ast")
        translate(ast)
        var asm = ""
        if (data.isNotEmpty() || utilData.isNotEmpty()) {
            asm += ".data\n\n"
        }

        asm += joinAsm(data.values) +
            joinAsm(utilData.values) +
            "\n.text\n\n.global main\n" +
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
     * The AST representation decouples the statements from a SequenceAST to a mapping of lists of
     * StatementAST in the symbol table to avoid stack overflow from recursing a huge block of
     * statements.
     */
    @TranslatorMethod(AST::class)
    private fun translateProgram(program: AST) {
        log("Translating program")
        resultRegister = R4

        // Translate all function blocks into assembly
        val functionASTs = program.statements.filterIsInstance<FunctionDeclarationAST>()
        functionASTs.forEach { translate(it) }

        // Translate main instructions into assembly
        mainPrologue(program.symbolTable)
        val statementASTs = program.statements.filter { it !is FunctionDeclarationAST }
        statementASTs.forEach { translate(it) }
        mainEpilogue(program.symbolTable)
    }

    @TranslatorMethod(FunctionDeclarationAST::class)
    private fun translateFunctionDeclaration(node: FunctionDeclarationAST) {
        log("Translating function declaration")
        // Define label
        val funcLabel = newFunctionLabel(node.funcName)
        currentLabel = funcLabel

        // Translate block statements and add to loop label - we start from register R4
        resultRegister = R4

        node.formals.forEach { it.ident.stackPosition }

        // TODO: issue - interdependence of statements to be addressed

        // Sets up the environment for a function
        functionPrologue(node, node.paramSymbolTable)

        node.statements.map { translate(it) }

        // Restore the state so that the program can resume from where it left off
        functionEpilogue(node)
    }

    @TranslatorMethod(CallAST::class)
    private fun translateCall(node: CallAST) {
        log("Translating call")

        var callStackSize = 0

        val oldOffset = offsetStackStore[0]

        // Set up the stack with the necessary parameters before a function call.
        node.actuals.reversed().forEach {
            // Load variable to resultRegister
            translate(it)
            val size = it.type.size()
            callStackSize += size
            addLines(
                // Allocate space in stack
                Sub(SP, SP, IntImmediateOperand(size)),
                // Move value from resultRegister to stack
                if (size == BYTE) {
                    StoreByte(resultRegister, ZeroOffset(SP))
                } else {
                    StoreWord(resultRegister, ZeroOffset(SP))
                }
            )
            offsetStackStore[0] += size
        }

        offsetStackStore[0] = oldOffset

        // Perform the call
        addLines(BranchLink(branchToFunction(node.funcName)))

        // Increment the stack pointer back to where it was
        if (callStackSize > 0) {
            addLines(Add(SP, SP, IntImmediateOperand(callStackSize)))
        }

        // Move the result from R0 to resultRegister
        addLines(Move(resultRegister, R0))
    }

    @TranslatorMethod(VariableDeclarationAST::class)
    private fun translateVariableDeclaration(node: VariableDeclarationAST) {
        // Parse the expression whose value is to be stored in the variable
        log("Translating VariableDeclarationAST")
        translate(node.rhs)
        transAssign(node.varIdent, node.symbolTable)
    }

    @TranslatorMethod(AssignToIdentAST::class)
    private fun translateAssignToIdent(node: AssignToIdentAST) {
        // Parse the expression whose value is to be stored in the variable
        log("Translating AssignToIdentAST")
        translate(node.rhs)
        val lhs = node.lhs
        transAssign(lhs.ident, lhs.symbolTable)
    }

    @TranslatorMethod(FreeStatementAST::class)
    private fun translateFreeStatement(node: FreeStatementAST) {
        log("Translating FreeStatementAST")
        defineUtilFuncs(P_FREE_PAIR)
        val variable = node.expr as VariableIdentifierAST
        transRetrieve(variable.ident, node.symbolTable)
        addLines(
            Move(R0, resultRegister),
            BranchLink(P_FREE_PAIR)
        )
    }

    /**
     * return statements have the format "return x". It can
     * only exist in a body of a non-main function and is used to return a value from
     * that function.
     */
    @TranslatorMethod(ReturnStatementAST::class)
    private fun translateReturnStatement(node: ReturnStatementAST) {
        log("Translating ReturnStatementAST")
        translate(node.expr)
        addLines(
            Move(R0, resultRegister)
        )
        unwindStack(node.parent!!.symbolTable)
        addLines(
            Pop(PC)
        )
    }

    /**
     * Per WACC language spec, exit statements have the format "exit x", where x is
     * an exit code of type int in range [0, 255].
     */
    @TranslatorMethod(ExitStatementAST::class)
    private fun translateExitStatement(node: ExitStatementAST) {
        log("Translating ExitStatementAST")
        translate(node.expr)
        addLines(
            Move(R0, resultRegister),
            BranchLink(EXIT)
        )
    }

    @TranslatorMethod(PrintStatementAST::class)
    private fun translatePrintStatement(node: PrintStatementAST) {
        log("Translating PrintStatementAST")
        translate(node.expr)
        addLines(
            Move(R0, resultRegister)
        )

        val expr = node.expr
        when (val type = expr.type) {
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
                if (type.elementType == CharType) {
                    defineUtilFuncs(P_PRINT_STRING)
                    addLines(BranchLink(P_PRINT_STRING))
                } else {
                    defineUtilFuncs(P_PRINT_REFERENCE)
                    addLines(BranchLink(P_PRINT_REFERENCE))
                }
            }
        }
    }

    @TranslatorMethod(PrintlnStatementAST::class)
    private fun translatePrintlnStatement(node: PrintlnStatementAST) {
        log("Translating PrintlnStatementAST")
        val printStatementAST = PrintStatementAST(node.parent!!, node.symbolTable, node.expr)
        translatePrintStatement(printStatementAST)
        defineUtilFuncs(P_PRINT_LN)
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
    private fun translateReadStatement(node: ReadStatementAST) {
        log("Translating ReadStatementAST")

        val assignTo = node.target.lhs
        val readFunc = if (node.target.type == IntType) P_READ_INT else P_READ_CHAR

        defineUtilFuncs(readFunc)
        when (assignTo) {
            is VariableIdentifierAST -> {
                getAddress(assignTo)
            }
            is ArrayElemAST -> {
                getAddress(assignTo)
            }
            is PairElemAST -> {
                getAddress(assignTo)
            }
            else -> { throw IllegalArgumentException("cannot read into expression of this type") }
        }
        addLines(
            Move(R0, resultRegister),
            BranchLink(readFunc)
        )
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
    private fun translateIfBlock(stat: IfBlockAST) {
        log("Translating IfBlockAST")

        // Add instructions to currentLabel
        translate(stat.condExpr)

        val elseLabel = newBranchLabel()
        val fiLabel = newBranchLabel()

        // compare to set condition flags and branch (can be optimised)
        addLines(
            Compare(resultRegister, IntImmediateOperand(0)),
            Branch(EQ, BranchLabelOperand(elseLabel))
        )

        // add sequence of instructions in THEN block under if label
        blockPrologue(stat)
        stat.statements.forEach(::translate)
        blockEpilogue(stat)

        // add branch to fi label
        addLines(
            Branch(BranchLabelOperand(fiLabel))
        )

        // add sequence of instructions in ELSE block under else label
        currentLabel = elseLabel
        blockPrologue(stat.elseBlock)
        stat.elseBlock.statements.forEach(::translate)
        blockEpilogue(stat.elseBlock)
        currentLabel = fiLabel
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
    private fun translateWhileBlock(node: WhileBlockAST) {
        log("Translating WhileBlockAST")

        val oldLabel = currentLabel

        // Translate block statements and add to loop label
        val loopLabel = newBranchLabel()
        currentLabel = loopLabel
        blockPrologue(node)
        node.statements.forEach { translate(it) }
        blockEpilogue(node)

        val checkLabel = newBranchLabel()
        currentLabel = oldLabel

        // Add branch instruction
        addLines(
            Branch(BranchLabelOperand(checkLabel))
        )

        // Translate condition statements and add to check label
        currentLabel = checkLabel
        translate(node.condExpr)

        // Add compare and branch instruction
        addLines(
            Compare(resultRegister, IntImmediateOperand(1)),
            Branch(EQ, BranchLabelOperand(loopLabel))
        )
    }

    @TranslatorMethod(BeginEndBlockAST::class)
    private fun translateBeginEndBlock(node: BeginEndBlockAST) {
        log("Translating BeginEndBlockAST")
        blockPrologue(node)
        node.statements.forEach { translate(it) }
        blockEpilogue(node)
    }

    @TranslatorMethod(NewPairAST::class)
    private fun translateNewPair(node: NewPairAST) {
        log("Translating NewPairAST")

        // Allocate two registers for newPair
        var accumulatorState = false
        if (resultRegister == Register.maxReg()) {
            resultRegister = resultRegister.prevReg()
            addLines(Push(resultRegister))
            offsetStackStore[0] += WORD
            accumulatorState = true
        }

        addLines(
            LoadWord(R0, PseudoImmediateOperand(2 * WORD)),
            BranchLink(MALLOC),
            Move(resultRegister, R0) // R_n
        )

        listOf(node.fstExpr, node.sndExpr).forEachIndexed { index, expr ->
            // Translate the expression and store it in memory with malloc
            resultRegister = resultRegister.nextReg() // R_n+1
            translate(expr) // result stored in // R_n+1
            addLines(
                LoadWord(R0, PseudoImmediateOperand(expr.type.size())),
                BranchLink(MALLOC)
            )

            // Store the value of the item of the pair in the address received from malloc
            addLines(
                when (expr.type.size()) {
                    WORD -> StoreWord(resultRegister, ZeroOffset(R0)) // R_n+1
                    else -> StoreByte(resultRegister, ZeroOffset(R0))
                }
            )

            // Store the address of the pair item into the actual pairs memory
            addLines(
                StoreWord(
                    R0,
                    if (index == 0) {
                        ZeroOffset(resultRegister.prevReg())
                    } else {
                        ImmediateOffset(resultRegister.prevReg(), index * WORD)
                    }
                ),
            )
            // Change back to R_n
            resultRegister = resultRegister.prevReg()
        }

        // result stored in MAX_REG - 1, we move the result to MAX_REG and restore
        // original value in MAX_REG - 1
        if (accumulatorState) {
            addLines(
                Move(resultRegister.nextReg(), ZeroOffset(resultRegister)),
                Pop(resultRegister)
            )
            resultRegister = resultRegister.nextReg()
            offsetStackStore[0] -= WORD
        }
    }

    //endregion

    //region translateExpr

    @TranslatorMethod(IntLiteralAST::class)
    private fun translateIntLiteral(node: IntLiteralAST) {
        log("Translating IntLiteralAST")
        addLines(
            LoadWord(resultRegister, PseudoImmediateOperand(node.intValue))
        )
    }

    @TranslatorMethod(BoolLiteralAST::class)
    private fun translateBoolLiteral(node: BoolLiteralAST) {
        log("Translating BoolLiteralAST")
        addLines(
            Move(resultRegister, BoolImmediateOperand(node.boolValue))
        )
    }

    @TranslatorMethod(CharLiteralAST::class)
    private fun translateCharLiteral(node: CharLiteralAST) {
        log("Translating CharLiteralAST")
        addLines(
            Move(resultRegister, CharImmediateOperand(node.charValue))
        )
    }

    @TranslatorMethod(StringLiteralAST::class)
    private fun translateStringLiteral(node: StringLiteralAST) {
        log("Translating StringLiteralAST")
        addLines(
            LoadWord(resultRegister, DataLabelOperand(newStringLabel(node.stringValue)))
        )
    }

    @TranslatorMethod(VariableIdentifierAST::class)
    private fun translateVariableIdentifier(node: VariableIdentifierAST) {
        log("Translating VariableIdentifierAST")
        transRetrieve(node.ident, node.symbolTable)
    }

    @TranslatorMethod(NullPairLiteralAST::class)
    @Suppress("UNUSED_PARAMETER")
    private fun translateNullPairLiteralAST(node: NullPairLiteralAST) {
        log("Translating NullPairLiteralAST")
        addLines(
            LoadWord(resultRegister, PseudoImmediateOperand(0))
        )
    }

    @TranslatorMethod(ArrayLiteralAST::class)
    private fun translateArrayLiteral(node: ArrayLiteralAST) {
        log("Translating ArrayLiteralAST")
        val elems = node.elems
        val elemSize: Int = if (node.elems.isNotEmpty()) {
            elems[0].type.size()
        } else {
            0
        }
        val size = WORD + elems.size * elemSize // calculate bytes need to malloc

        // Allocate two registers for ArrayLiteral
        var accumulatorState = false
        if (resultRegister == Register.maxReg()) {
            resultRegister = resultRegister.prevReg()
            addLines(Push(resultRegister))
            offsetStackStore[0] += WORD
            accumulatorState = true
        }

        currentLabel.addLines(
            LoadWord(R0, PseudoImmediateOperand(size)),
            BranchLink(MALLOC),
            Move(resultRegister, R0)
        )
        // offset (in bytes) from the malloc'ed address to store the value at
        // starts at addr + 4 because the first 4 bytes are dedicated to storing tha array's length
        var offset = WORD
        for (expr in elems) {
            val dest = resultRegister.nextReg()
            val src = resultRegister
            resultRegister = dest // R_n+1
            translate(expr)
            resultRegister = src // R_n
            currentLabel.addLines(
                if (expr.type.size() == WORD) {
                    StoreWord(dest, ImmediateOffset(src, offset))
                } else {
                    StoreByte(dest, ImmediateOffset(src, offset))
                }
            )
            offset += expr.type.size()
        }

        val dest = resultRegister.nextReg() // R_n+1
        currentLabel.addLines(
            LoadWord(dest, PseudoImmediateOperand(elems.size)),
            StoreWord(dest, ZeroOffset(resultRegister))
        )

        // result stored in MAX_REG - 1, we move the result to MAX_REG and restore
        // original value in MAX_REG - 1
        if (accumulatorState) {
            addLines(
                Move(dest, ZeroOffset(resultRegister)),
                Pop(resultRegister)
            )
            resultRegister = dest
            offsetStackStore[0] -= WORD
        }
    }

    @TranslatorMethod(ArrayElemAST::class)
    private fun translateArrayElem(node: ArrayElemAST) {
        log("Translating ArrayElemAST")

        val oldReg = resultRegister

        defineUtilFuncs(
            P_CHECK_ARRAY_BOUNDS,
            P_THROW_RUNTIME_ERROR
        )

        // Allocate two registers for arrayElem
        var accumulatorState = false
        if (resultRegister == Register.maxReg()) { // R10
            resultRegister = resultRegister.prevReg() // R9
            addLines(Push(resultRegister)) // Spare R9
            offsetStackStore[0] += WORD
            accumulatorState = true
        }

        // load address of value into resultRegister
        getAddress(node)

        if (node.elemType.size() == WORD) {
            addLines(
                // put whats at that index into result reg
                LoadWord(resultRegister, ZeroOffset(resultRegister))
            )
        } else {
            addLines(
                LoadByte(resultRegister, ZeroOffset(resultRegister))
            )
        }

        // result stored in MAX_REG - 1, we move the result to MAX_REG and restore
        // original value in MAX_REG - 1
        if (accumulatorState) {
            addLines(
                Move(resultRegister.nextReg(), ZeroOffset(resultRegister)),
                Pop(resultRegister)
            )
            resultRegister = resultRegister.nextReg()
            offsetStackStore[0] -= WORD
        }

        resultRegister = oldReg
    }

    @TranslatorMethod(AssignToArrayElemAST::class)
    private fun translateAssignToArrayElem(node: AssignToArrayElemAST) {
        log("Translating AssignToArrayElemAST")

        val oldReg = resultRegister

        defineUtilFuncs(
            P_CHECK_ARRAY_BOUNDS,
            P_THROW_RUNTIME_ERROR
        )

        val arrayElemAST = node.lhs
        val arrayVariable = node.lhs.arrayVar
        val typeSize = (arrayVariable.type as ArrayType).elementType.size()
        val stackPointerOffset = arrayVariable.ident.stackPosition

        addLines(
            Add(resultRegister.nextReg(), SP, IntImmediateOperand(stackPointerOffset))
        )

        translate(node.rhs)

        resultRegister = resultRegister.nextReg()
        for (indexExpr in arrayElemAST.indexExpr) {
            resultRegister = resultRegister.nextReg()
            translate(indexExpr)
            resultRegister = resultRegister.prevReg()
            addLines(
                LoadWord(
                    resultRegister,
                    ZeroOffset(resultRegister)
                ),
                Move(R0, resultRegister.nextReg()),
                Move(R1, resultRegister),
                BranchLink(P_CHECK_ARRAY_BOUNDS),
                Add(
                    resultRegister,
                    resultRegister,
                    IntImmediateOperand(4)
                ),
                if (typeSize == WORD) {
                    Add(
                        resultRegister,
                        resultRegister,
                        LogicalShiftLeft(resultRegister.nextReg(), 2)
                    )
                } else {
                    Add(resultRegister, resultRegister, resultRegister.nextReg())
                }
            )
        }
        resultRegister = oldReg
        val dest = ZeroOffset(resultRegister.nextReg())
        addLines(
            if (typeSize == WORD) {
                StoreWord(resultRegister, dest)
            } else {
                StoreByte(resultRegister, dest)
            }
        )
    }

    @TranslatorMethod(FstPairElemAST::class)
    private fun transFstPairElem(node: FstPairElemAST) {
        log("Translating FstPairElemAST")
        translatePairElem(node)
    }

    @TranslatorMethod(SndPairElemAST::class)
    private fun translateSndPairElem(node: SndPairElemAST) {
        log("Translating SndPairElemAST")
        translatePairElem(node)
    }

    private fun translatePairElem(node: PairElemAST) {
        getAddress(node)
        assert(node.expr.type.size() == WORD)
        addLines(LoadWord(resultRegister, ZeroOffset(resultRegister)))
    }

    @TranslatorMethod(AssignToPairElemAST::class)
    private fun translateAssignToPairElem(node: AssignToPairElemAST) {
        log("Translating AssignToPairElemAST")
        defineUtilFuncs(
            P_CHECK_NULL_POINTER
        )
        translate(node.rhs)
        val pairStackOffset = (node.lhs.expr as VariableIdentifierAST).ident.stackPosition
        val storeInstruction = when (node.rhs.type.size()) {
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

    @TranslatorMethod(UnaryOpExprAST::class)
    private fun translateUnOp(unOpExpr: UnaryOpExprAST) {
        log("Translating UnaryOpExprAST")
        translate(unOpExpr.expr)

        when (unOpExpr.operator) {
            UnaryOp.BANG -> {
                addLines(
                    Xor(resultRegister, resultRegister, IntImmediateOperand(1))
                )
            }
            UnaryOp.MINUS -> {
                defineUtilFuncs(
                    P_CHECK_DIVIDE_BY_ZERO,
                    P_THROW_RUNTIME_ERROR,
                    P_THROW_OVERFLOW_ERROR,
                    P_PRINT_STRING
                )
                addLines(
                    ReverseSub(
                        updateFlags = true, resultRegister, resultRegister,
                        IntImmediateOperand(0)
                    ),
                    BranchLink(VS, P_THROW_OVERFLOW_ERROR)
                )
            }
            UnaryOp.LEN -> {
                addLines(
                    LoadWord(resultRegister, ZeroOffset(resultRegister))
                )
            }
            else -> { }
        }
    }

    @TranslatorMethod(BinaryOpExprAST::class)
    private fun translateBinOp(expr: BinaryOpExprAST) {
        log("Translating BinaryOpExprAST")

        // Allocate two registers for BinOp
        var accumulatorState = false
        if (resultRegister == Register.maxReg()) {
            resultRegister = resultRegister.prevReg()
            addLines(Push(resultRegister))
            offsetStackStore[0] += WORD
            accumulatorState = true
        }

        translate(expr.expr1)

        resultRegister = resultRegister.nextReg()
        translate(expr.expr2)
        resultRegister = resultRegister.prevReg()
        when (expr.operator) {
            PLUS, MINUS, MULT, DIV, MOD -> {
                defineUtilFuncs(
                    P_THROW_RUNTIME_ERROR,
                    P_THROW_OVERFLOW_ERROR,
                    P_PRINT_STRING
                )

                when (expr.operator) {
                    MULT -> {
                        addLines(
                            LongMult(
                                updateFlags = true,
                                resultRegister,
                                resultRegister.nextReg(),
                                resultRegister,
                                resultRegister.nextReg()
                            ),
                            Compare(
                                resultRegister.nextReg(),
                                ArithmeticShiftRight(resultRegister, 31)
                            ),
                            BranchLink(NE, P_THROW_OVERFLOW_ERROR)
                        )
                    }
                    DIV -> {
                        defineUtilFuncs(P_CHECK_DIVIDE_BY_ZERO)
                        addLines(
                            Move(R0, resultRegister),
                            Move(R1, resultRegister.nextReg()),
                            BranchLink(P_CHECK_DIVIDE_BY_ZERO),
                            BranchLink(AEABI_IDIV),
                            Move(resultRegister, R0)
                        )
                    }
                    MOD -> {
                        defineUtilFuncs(P_CHECK_DIVIDE_BY_ZERO)
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
                            BranchLink(VS, P_THROW_OVERFLOW_ERROR)
                        )
                    }
                    MINUS -> {
                        addLines(
                            Sub(true, resultRegister, resultRegister, resultRegister.nextReg()),
                            BranchLink(VS, P_THROW_OVERFLOW_ERROR)
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
                            Move(GT, resultRegister, IntImmediateOperand(1)),
                            Move(LE, resultRegister, IntImmediateOperand(0))
                        )
                    }
                    GTE -> {
                        addLines(
                            Move(GE, resultRegister, IntImmediateOperand(1)),
                            Move(LT, resultRegister, IntImmediateOperand(0))
                        )
                    }
                    BinaryOp.LT -> {
                        addLines(
                            Move(LT, resultRegister, IntImmediateOperand(1)),
                            Move(GE, resultRegister, IntImmediateOperand(0))
                        )
                    }
                    LTE -> {
                        addLines(
                            Move(LE, resultRegister, IntImmediateOperand(1)),
                            Move(GT, resultRegister, IntImmediateOperand(0))
                        )
                    }
                    EQUALS -> {
                        addLines(
                            Move(EQ, resultRegister, IntImmediateOperand(1)),
                            Move(NE, resultRegister, IntImmediateOperand(0))
                        )
                    }
                    NOT_EQUALS -> {
                        addLines(
                            Move(NE, resultRegister, IntImmediateOperand(1)),
                            Move(EQ, resultRegister, IntImmediateOperand(0))
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

        // result stored in MAX_REG - 1, we move the result to MAX_REG and restore
        // original value in MAX_REG - 1
        if (accumulatorState) {
            addLines(
                Move(resultRegister.nextReg(), ZeroOffset(resultRegister)),
                Pop(resultRegister)
            )
            resultRegister = resultRegister.nextReg()
            offsetStackStore[0] -= WORD
        }
    }

    //endregion

    //region decorators

    /**
     * Sets up the environment for the block of statements, specifically by initialising the stack variables
     */
    private fun blockPrologue(node: BlockAST) {
        blockPrologue(node.symbolTable)
    }

    private fun blockPrologue(symbolTable: SymbolTable) {
        log("Calling blockPrologue")
        // Push the value that tracks the stack space used by intermediate values
        // by the current block
        offsetStackStore.addFirst(0)

        // Calculate how much space to be allocated (and modify each variable to include its position on the stack)
        val stackSpaceUsed = symbolTable.getStackSize()
        if (stackSpaceUsed > 0) {
            var currentStackPosition = stackSpaceUsed
            var subtractLeft = stackSpaceUsed

            // Setup stack
            while (subtractLeft > 0) {
                val subtractNow = if (subtractLeft <= MAX_STACK_CHANGE) {
                    subtractLeft
                } else {
                    MAX_STACK_CHANGE
                }
                addLines(Sub(SP, SP, IntImmediateOperand(subtractNow)))
                subtractLeft -= subtractNow
            }

            // Calculate the stack position for each variable
            val variables = symbolTable.getValuesByType(Variable::class)
            for (v in variables) {
                currentStackPosition -= v.type.size()
                v.stackPosition = currentStackPosition
            }
        }
    }

    /**
     * Restores the state so that the program can resume from where it left off before entering the block
     */
    private fun blockEpilogue(node: BlockAST) {
        blockEpilogue(node.symbolTable)
    }

    private fun blockEpilogue(symbolTable: SymbolTable) {
        log("Calling blockEpilogue")
        // Pop the value that tracks the stack space used by intermediate values
        // by the current block
        offsetStackStore.removeFirst()

        unwindStack(symbolTable)
    }

    private fun unwindStack(symbolTable: SymbolTable) {
        val stackSpaceUsed = symbolTable.getStackSize()
        if (stackSpaceUsed > 0) {
            var addLeft = stackSpaceUsed

            val subList = mutableListOf<Instruction>()

            // Setup stack
            while (addLeft > 0) {
                val addNow = if (addLeft <= MAX_STACK_CHANGE) {
                    addLeft
                } else {
                    MAX_STACK_CHANGE
                }
                subList.add(Add(SP, SP, IntImmediateOperand(addNow)))
                addLeft -= addNow
            }

            addLines(subList.reversed())
        }
    }

    private fun functionPrologue(
        node: FunctionDeclarationAST,
        paramSymbolTable: SymbolTable? = null
    ) {
        functionPrologue(node.symbolTable, paramSymbolTable)
    }

    private fun functionPrologue(symbolTable: SymbolTable, paramSymbolTable: SymbolTable? = null) {
        log("Calling functionPrologue")
        addLines(
            Push(LR)
        )
        if (paramSymbolTable != null) {
            // Params are pushed in reverse order to the stack
            // After the last param is pushed, the value of LR is pushed, which is a word
            // So the stack position of the first parameter is WORD
            var currentStackPos = WORD
            paramSymbolTable.getValuesByType(Param::class).forEach {
                it.stackPosition = currentStackPos
                currentStackPos += it.type.size()
            }
            assert(currentStackPos == paramSymbolTable.getStackSize() + WORD)
        }
        blockPrologue(symbolTable)
    }

    private fun functionEpilogue(node: FunctionDeclarationAST) {
        functionEpilogue(node.symbolTable)
    }

    private fun functionEpilogue(symbolTable: SymbolTable) {
        log("Calling functionEpilogue")
        blockEpilogue(symbolTable)
        addLines(
            Pop(PC),
            LTORG
        )
    }

    private fun mainPrologue(symbolTable: SymbolTable) {
        val mainLabel = newBranchLabel("main")
        currentLabel = mainLabel
        functionPrologue(symbolTable)
    }

    private fun mainEpilogue(symbolTable: SymbolTable) {
        blockEpilogue(symbolTable)
        addLines(
            LoadWord(R0, PseudoImmediateOperand(0)),
            Pop(PC),
            LTORG
        )
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
        log("Generating branch label: $name")
        val label = BranchLabel(name, *lines)
        text[label.name] = label
        return label
    }

    private fun newFunctionLabel(funcName: String, vararg lines: Instruction): BranchLabel {
        return newBranchLabel("f_$funcName", *lines)
    }

    private fun branchToFunction(funcName: String): BranchLabelOperand {
        return BranchLabelOperand("f_$funcName")
    }

    private fun defineUtilFuncs(vararg funcs: UtilFunction) {
        funcs.forEach { func ->
            if (!utilText.containsKey(func.labelName)) {
                defineUtilFuncs(*func.dependencies)
                log("Adding util function: ${func.name}")
                utilText[func.labelName] = func.labelBlock
                func.dataBlocks.forEach {
                    utilData[it.name] = it
                }
            }
        }
    }

    private fun transAssign(variable: Variable, currentScope: SymbolTable) {
        log("Calling transAssign")
        variableAction(variable, currentScope, store = true)
    }

    private fun transRetrieve(variable: Variable, currentScope: SymbolTable) {
        log("Calling transRetrieve")
        variableAction(variable, currentScope, store = false)
    }

    private fun variableAction(
        variable: Variable,
        currentScope: SymbolTable,
        store: Boolean
    ) {
        val scopeOffset = currentScope.calcScopeOffset(variable)
        val size = variable.type.size()

        // Consider the case where intermediate results are pushed on the stack
        val pos = variable.stackPosition + offsetStackStore[0] + scopeOffset
        val addressOperand = if (pos == 0) {
            ZeroOffset(SP)
        } else {
            ImmediateOffset(SP, pos)
        }

        if (store) {
            addLines(
                if (size == BYTE) {
                    StoreByte(resultRegister, addressOperand)
                } else {
                    StoreWord(resultRegister, addressOperand)
                }
            )
        } else {
            addLines(
                if (size == BYTE) {
                    LoadByte(resultRegister, addressOperand)
                } else {
                    LoadWord(resultRegister, addressOperand)
                }
            )
        }
    }

    private fun getAddress(variable: VariableIdentifierAST) {
        addLines(
            Add(resultRegister, SP, IntImmediateOperand(variable.ident.stackPosition))
        )
    }

    private fun getAddress(arrayElem: ArrayElemAST) {
        val variable = arrayElem.arrayVar.ident

        addLines(Add(resultRegister, SP, IntImmediateOperand(variable.stackPosition)))

        for (i in arrayElem.indexExpr.indices) {
            val index = arrayElem.indexExpr[i]

            resultRegister = resultRegister.nextReg()
            translate(index)
            resultRegister = resultRegister.prevReg()

            addLines(
                LoadWord(resultRegister, ZeroOffset(resultRegister)),
                // check bounds of array
                Move(R0, resultRegister.nextReg()),
                Move(R1, resultRegister),
                BranchLink(P_CHECK_ARRAY_BOUNDS),
                Add(resultRegister, resultRegister, IntImmediateOperand(WORD))
            )
            addLines(
                // get address of desired index into result reg
                Add(
                    resultRegister,
                    resultRegister,
                    LogicalShiftLeft(resultRegister.nextReg(), 2)
                )
            )
        }
    }

    private fun getAddress(pairElemAST: PairElemAST) {
        defineUtilFuncs(
            P_CHECK_NULL_POINTER
        )
        // TODO: Fix. pairElemAST.expr is not always VariableIdentifierAST
        val pairPointerOffset = (pairElemAST.expr as VariableIdentifierAST).ident.stackPosition
        val sizeOfFstElem = (pairElemAST.expr.type as PairType).fstType.size()
        addLines(
            LoadWord(resultRegister, ImmediateOffset(SP, pairPointerOffset)),
            Move(R0, resultRegister),
            BranchLink(P_CHECK_NULL_POINTER),
            LoadWord(
                resultRegister,
                if (pairElemAST is FstPairElemAST) {
                    ZeroOffset(resultRegister)
                } else {
                    ImmediateOffset(resultRegister, sizeOfFstElem)
                }
            )
        )
    }

    private fun log(str: String) {
        if (enableLogging) {
            println(str)
        }
    }

    //endregion
}
