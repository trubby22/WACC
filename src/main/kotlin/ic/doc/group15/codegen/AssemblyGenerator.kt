package ic.doc.group15.codegen

import ic.doc.group15.BYTE
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
import java.util.*
import kotlin.reflect.KCallable
import kotlin.reflect.KClass
import kotlin.reflect.jvm.isAccessible

private typealias TranslatorMap = Map<KClass<out ASTNode>, KCallable<*>>

class AssemblyGenerator(private val ast: AST) {

    val MAX_STACK_CHANGE = 1024

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
        println("Translating ast")
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
     * Sets up the environment for the block of statements, specifically by initialising the stack variables
     */
    private fun blockPrologue(node: BlockAST) {
        println("Calling blockPrologue")
        // Push the value that tracks the stack space used by intermediate values
        // by the current block
        offsetStackStore.addFirst(0)

        // Calculate how much space to be allocated (and modify each variable to include its position on the stack)
        val stackSpaceUsed = node.symbolTable.getStackSize()
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
                addLines(Sub(SP, SP, ImmediateOperand(subtractNow)))
                subtractLeft -= subtractNow
            }

            // Calculate the stack position for each variable
            val variables = node.symbolTable.getValuesByType(Variable::class)
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
        println("Calling blockEpilogue")
        // Pop the value that tracks the stack space used by intermediate values
        // by the current block
        offsetStackStore.removeFirst()

        // Unwind stack
        val stackSpaceUsed = node.symbolTable.getStackSize()
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
                subList.add(Add(SP, SP, ImmediateOperand(addNow)))
                addLeft -= addNow
            }

            addLines(subList.reversed())
        }
    }

    /**
     * Sets up the environment for a function
     */
    private fun functionPrologue(node: FunctionDeclarationAST) {
        println("Calling functionPrologue")
        addLines(Push(LR))
    }

    /**
     * Restores the state so that the program can resume from where it left off before entering the function
     */
    private fun functionEpilogue(node: FunctionDeclarationAST) {
        println("Calling functionEpilogue for function: ${node.funcName}")
        if (node.funcName == "main") {
            // Add return statement (main function implicitly returns 0)
            translate(
                ReturnStatementAST(
                    node, node.symbolTable.subScope(),
                    IntLiteralAST(0)
                )
            )
        }
        addLines(
            LTORG
        )
    }

    /**
     * Sets up the stack with the necessary parameters before a function call.
     */
    private fun functionCallPrologue(node: CallAST) {
        println("Calling functionCallPrologue")
        node.actuals.forEach {
            // Load variable to resultRegister
            translate(it)
            val size = it.type.size()
            addLines(
                // Allocate space in stack
                Sub(SP, SP, ImmediateOperand(it.type.size())),
                // Move value from resultRegister to stack
                if (size == BYTE) {
                    StoreByte(resultRegister, ZeroOffset(SP))
                } else {
                    StoreWord(resultRegister, ZeroOffset(SP))
                }
            )
        }
    }

    /**
     * Restores the state so that the program can resume from where it left off before the
     * function sub-call was made
     */
    private fun functionCallEpilogue(node: CallAST) {
        println("Calling functionCallEpilogue")
        // Increment the stack pointer back to where it was
        val stackSpaceUsed = node.actuals.sumOf { arg -> arg.type.size() }

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
        println("Translating program")
        println("program.statements:")
        program.statements.forEach { println(it) }
        resultRegister = R4
        val functionASTs = program.statements.filterIsInstance<FunctionDeclarationAST>()

        // translate all function blocks into assembly
        functionASTs.forEach { f -> translate(f) }

        // Translate main instructions into assembly
        // We create a new FunctionDeclarationAST to store statements in the main function
        // and let transFunction add the entries to the text attribute

        // The symbol table contains the statements AND the FunctionDeclarationAST
        val mainAST = FunctionDeclarationAST(program, program.symbolTable, IntType, "main")
        mainAST.funcIdent = FunctionType(IntType, emptyList(), program.symbolTable)

        // Add statements
        val statementASTs = program.statements.filter { s -> s !is FunctionDeclarationAST }
        mainAST.statements.addAll(statementASTs)

        translate(mainAST)
    }

    @TranslatorMethod(FunctionDeclarationAST::class)
    private fun transFunctionDeclaration(node: FunctionDeclarationAST) {
        println("Translating function declaration")
        // Define label
        val funcLabel = newBranchLabel(node.funcName)
        currentLabel = funcLabel

        // Translate block statements and add to loop label - we start from register R4
        resultRegister = R4
        // TODO: issue - interdependence of statements to be addressed
        functionPrologue(node)
        blockPrologue(node)
        node.statements.map { s -> translate(s) }
        blockEpilogue(node)
        functionEpilogue(node)
    }

    @TranslatorMethod(CallAST::class)
    private fun transCall(node: CallAST) {
        println("Translating call")
        functionCallPrologue(node)
        addLines(BranchLink(BranchLabelOperand("f_${node.funcName}")))
        functionCallEpilogue(node)
        // Move the result from R0 to resultRegister
        addLines(Move(resultRegister, R0))
    }

    @TranslatorMethod(VariableDeclarationAST::class)
    private fun transVariableDeclaration(node: VariableDeclarationAST) {
        // Parse the expression whose value is to be stored in the variable
        println("Translating VariableDeclarationAST")
        translate(node.rhs)
        transAssign(node.varIdent)
    }

    @TranslatorMethod(AssignToIdentAST::class)
    private fun transAssignToIdent(node: AssignToIdentAST) {
        // Parse the expression whose value is to be stored in the variable
        println("Translating AssignToIdentAST")
        translate(node.rhs)
        transAssign(node.lhs.ident)
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
        println("Translating WhileBlockAST")
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
        blockPrologue(node)
        node.statements.forEach { translate(it) }
        blockEpilogue(node)
        text[currentLabel.name] = currentLabel

        // Translate condition statements and add to check label
        currentLabel = checkLabel
        translate(node.condExpr)

        // Add compare and branch instruction
        addLines(
            Compare(resultRegister, ImmediateOperand(1)),
            Branch(EQ, BranchLabelOperand(loopLabel))
        )
    }

    @TranslatorMethod(BeginEndBlockAST::class)
    fun transBeginEndBlock(node: BeginEndBlockAST) {
        println("Translating BeginEndBlockAST")
        node.statements.forEach { translate(it) }
    }

    @TranslatorMethod(FreeStatementAST::class)
    fun transFreeStatement(node: FreeStatementAST) {
        println("Translating FreeStatementAST")
        defineUtilFuncs(P_FREE_PAIR)
        val variable = (node.expr as VariableIdentifierAST).ident
        transRetrieve(variable)
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
    fun transReturnStatement(node: ReturnStatementAST) {
        println("Translating ReturnStatementAST")
        if (node.expr is IntLiteralAST) {
            addLines(
                LoadWord(R0, PseudoImmediateOperand(node.expr.intValue)),
                Pop(PC)
            )
        } else {
            translate(node.expr)
            addLines(
                Move(R0, resultRegister),
                Pop(PC)
            )
        }
    }

    /**
     * Per WACC language spec, exit statements have the format "exit x", where x is
     * an exit code of type int in range [0, 255].
     */
    @TranslatorMethod(ExitStatementAST::class)
    fun transExitStatement(node: ExitStatementAST) {
        println("Translating ExitStatementAST")
        translate(node.expr)
        addLines(
            Move(R0, resultRegister),
            BranchLink(EXIT)
        )
    }

    @TranslatorMethod(PrintStatementAST::class)
    fun transPrintStatement(node: PrintStatementAST) {
        println("Translating PrintStatementAST")
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
    fun transPrintlnStatement(node: PrintlnStatementAST) {
        println("Translating PrintlnStatementAST")
        val printStatementAST = PrintStatementAST(node.parent!!, node.symbolTable, node.expr)
        transPrintStatement(printStatementAST)
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
    fun transReadStatement(node: ReadStatementAST) {
        println("Translating ReadStatementAST")
        when (node.target.type) {
            IntType -> {
                defineUtilFuncs(P_READ_INT)
                translate(node.target.lhs)
                addLines(
                    Move(R0, resultRegister),
                    BranchLink(P_READ_INT)
                )
            }
            CharType -> {
                defineUtilFuncs(P_READ_CHAR)
                translate(node.target.lhs)
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
    fun transIfBlock(stat: IfBlockAST) {
        println("Translating IfBlockAST")
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
        println("Translating NewPairAST")
        addLines(
            LoadWord(R0, PseudoImmediateOperand(2 * WORD)),
            BranchLink(MALLOC),
            Move(resultRegister, R0)
        )

        listOf(node.fstExpr, node.sndExpr).forEachIndexed { index, expr ->
            // Translate the expression and store it in memory with malloc
            resultRegister = resultRegister.nextReg()
            translate(expr)
            addLines(
                LoadWord(R0, PseudoImmediateOperand(expr.type.size())),
                BranchLink(MALLOC)
            )

            // Store the value of the item of the pair in the address received from malloc
            addLines(
                when (expr.type.size()) {
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
        println("Translating IntLiteralAST")
        addLines(
            LoadWord(resultRegister, PseudoImmediateOperand(node.intValue))
        )
    }

    @TranslatorMethod(BoolLiteralAST::class)
    private fun translateBoolLiteral(node: BoolLiteralAST) {
        println("Translating BoolLiteralAST")
        addLines(
            Move(resultRegister, ImmediateOperand(node.boolValue))
        )
    }

    @TranslatorMethod(CharLiteralAST::class)
    private fun translateCharLiteral(node: CharLiteralAST) {
        println("Translating CharLiteralAST")
        addLines(
            Move(resultRegister, ImmediateOperand(node.charValue))
        )
    }

    @TranslatorMethod(StringLiteralAST::class)
    private fun translateStringLiteral(node: StringLiteralAST) {
        println("Translating StringLiteralAST")
        addLines(
            LoadWord(resultRegister, DataLabelOperand(newStringLabel(node.stringValue)))
        )
    }

    @TranslatorMethod(VariableIdentifierAST::class)
    private fun translateVariableIdentifier(node: VariableIdentifierAST) {
        println("Translating VariableIdentifierAST")
        transRetrieve(node.ident)
    }

    @TranslatorMethod(ArrayLiteralAST::class)
    private fun transArrayLiteral(node: ArrayLiteralAST) {
        println("Translating ArrayLiteralAST")
        val oldReg = resultRegister
        val elems = node.elems
        val elemSize: Int = if (node.elems.isNotEmpty()) {
            elems[0].type.size()
        } else {
            0
        }
        val size = 4 + elems.size * elemSize // calculate bytes need to malloc
        currentLabel.addLines(
            LoadWord(R0, PseudoImmediateOperand(size)),
            BranchLink(MALLOC),
            Move(resultRegister, R0)
        )
        var offset = 0 // now we go in this for loop to put all the items of the array into the memory of the array
        for (expr in elems) {
            resultRegister = resultRegister.nextReg()
            translate(expr)
            resultRegister = oldReg
            if (expr.type.size() == 4) {
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
            LoadWord(resultRegister.nextReg(), PseudoImmediateOperand(elems.size)),
            StoreWord(resultRegister.nextReg(), ZeroOffset(resultRegister))
        )
    }

    @TranslatorMethod(ArrayElemAST::class)
    private fun translateArrayElem(node: ArrayElemAST) {
        println("Translating ArrayElemAST")
        defineUtilFuncs(
            P_CHECK_ARRAY_BOUNDS,
            P_THROW_RUNTIME_ERROR
        )
        val variable = node.arrayVar.ident
        addLines(Add(resultRegister, SP, ImmediateOperand(variable.stackPosition)))

        for (i in node.indexExpr.indices) {
            val index = node.indexExpr[i]
            val currReg = resultRegister
            resultRegister = currReg.nextReg()
            translate(index) // translating the index of the array
            resultRegister = currReg
            addLines(
                LoadWord(resultRegister, ZeroOffset(resultRegister)),
                Move(R0, resultRegister.nextReg()), // this and next two lines just check bounds of array
                Move(R1, resultRegister),
                BranchLink(P_CHECK_ARRAY_BOUNDS),
                Add(resultRegister, resultRegister, ImmediateOperand(WORD))
            ) // move past array size
            if (i == node.indexExpr.lastIndex) {
                if (node.elemType.size() == WORD) {
                    addLines(
                        // get address of desired index into result reg
                        Add(resultRegister, resultRegister, LogicalShiftLeft(resultRegister.nextReg(), 2)),
                        // put whats at that index into result reg
                        LoadWord(resultRegister, ZeroOffset(resultRegister))
                    )
                } else {
                    addLines(
                        Add(resultRegister, resultRegister, resultRegister.nextReg()),
                        LoadByte(resultRegister, ZeroOffset(resultRegister))
                    )
                }
            } else {
                addLines(
                    // get address of desired index into result reg
                    Add(resultRegister, resultRegister, LogicalShiftLeft(resultRegister.nextReg(), 2)),
                    // put whats at that index into result reg
                    LoadWord(resultRegister, ZeroOffset(resultRegister))
                )
            }
        }
    }

    @TranslatorMethod(AssignToArrayElemAST::class)
    fun transAssignToArrayElem(node: AssignToArrayElemAST) {
        println("Translating AssignToArrayElemAST")
        defineUtilFuncs(
            P_CHECK_ARRAY_BOUNDS,
            P_THROW_RUNTIME_ERROR
        )
        translate(node.rhs)
        val arrayElemAST = node.lhs
//        TODO: calculate hardcodedNum's instead of
//         hardcoding them
        val arrayVariable = node.lhs.arrayVar
        val stackPointerOffset = arrayVariable.ident.stackPosition
        val hardcodedNum1 = 4
        val hardcodedNum2 = 2
        addLines(Add(resultRegister.nextReg(), SP, ImmediateOperand(stackPointerOffset)))
        for (indexExpr in arrayElemAST.indexExpr) {
            translate(indexExpr)
            addLines(
                LoadWord(
                    resultRegister.nextReg().nextReg(),
                    ZeroOffset(resultRegister.nextReg().nextReg())
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
        println("Translating FstPairElemAST")
        defineUtilFuncs(
            P_CHECK_NULL_POINTER
        )
        val pairPointerOffset = (node.expr as VariableIdentifierAST).ident.stackPosition
        val loadInstruction = if (node.pairExpr.type.size() == BYTE) {
            LoadByte(
                resultRegister,
                ZeroOffset
                    (resultRegister)
            )
        } else {
            LoadWord(
                resultRegister,
                ZeroOffset
                    (resultRegister)
            )
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
        println("Translating SndPairElemAST")
        defineUtilFuncs(
            P_CHECK_NULL_POINTER
        )
        val pairPointerOffset = (node.expr as VariableIdentifierAST).ident.stackPosition
        val sizeOfFstElem = (node.pairExpr.type as PairType).sndType.size()
        val loadInstruction = if (node.pairExpr.type.size() == BYTE) {
            LoadByte(
                resultRegister,
                ZeroOffset
                    (resultRegister)
            )
        } else {
            LoadWord(
                resultRegister,
                ZeroOffset
                    (resultRegister)
            )
        }
        addLines(
            LoadWord(resultRegister, ImmediateOffset(SP, pairPointerOffset)),
            Move(R0, resultRegister),
            BranchLink(P_CHECK_NULL_POINTER),
            LoadWord(resultRegister, ImmediateOffset(resultRegister, sizeOfFstElem)),
            loadInstruction
        )
    }

    @TranslatorMethod(AssignToPairElemAST::class)
    private fun transAssignToPairElem(node: AssignToPairElemAST) {
        println("Translating AssignToPairElemAST")
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

    @TranslatorMethod(BinaryOpExprAST::class)
    private fun transBinOp(expr: BinaryOpExprAST) {
        println("Translating BinaryOpExprAST")

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
                    P_CHECK_DIVIDE_BY_ZERO,
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

        resultRegister = resultRegister.prevReg()

        // Restore the value to MAX_REG - 1
        if (accumulatorState) {
            addLines(Pop(resultRegister))
            offsetStackStore[0] -= WORD
        }
    }

    @TranslatorMethod(UnaryOpExprAST::class)
    private fun transUnOp(unOpExpr: UnaryOpExprAST) {
        println("Translating UnaryOpExprAST")
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
                    P_THROW_OVERFLOW_ERROR,
                    P_PRINT_STRING
                )
                addLines(
                    ReverseSub(resultRegister, resultRegister, ImmediateOperand(0)),
                    BranchLink(P_THROW_OVERFLOW_ERROR)
                )
            }
            UnaryOp.LEN -> {
                addLines(
                    LoadWord(resultRegister, ZeroOffset(resultRegister))
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
                println("Adding util function: ${func.name}")
                utilText[func.labelName] = func.labelBlock
                func.dataBlocks.forEach {
                    utilData[it.name] = it
                }
            }
        }
    }

    private fun transAssign(variable: Variable) {
        println("Calling transAssign")
        val size = variable.type.size()
        val addressOperand = if (variable.stackPosition == 0) { ZeroOffset(SP) } else { ImmediateOffset(SP, size) }

        addLines(
            if (size == BYTE) {
                StoreByte(resultRegister, addressOperand)
            } else {
                StoreWord(resultRegister, addressOperand)
            }
        )
    }

    private fun transRetrieve(variable: Variable) {
        println("Calling transRetrieve")
        val size = variable.type.size()
        val addressOperand = if (variable.stackPosition == 0) {
            ZeroOffset(SP)
        } else {
            ImmediateOffset(SP, size)
        }

        addLines(
            if (size == BYTE) {
                LoadByte(resultRegister, addressOperand)
            } else {
                LoadWord(resultRegister, addressOperand)
            }
        )
    }

    //endregion
}
