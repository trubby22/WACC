package ic.doc.group15.ssa

import ic.doc.group15.assembly.UniqueLabelGenerator
import ic.doc.group15.type.BasicType
import ic.doc.group15.type.ReturnableType
import java.util.concurrent.atomic.AtomicInteger

interface Instruction
abstract class Value(val type: ReturnableType) {
    override fun toString(): String {
        return "<$type>"
    }
}

open class Register protected constructor(type: ReturnableType): Value(type) {
    companion object {
        private val counter = AtomicInteger()

        fun generate(type: ReturnableType) = Register(type)

        fun reset() {
            counter.set(0)
        }
    }

    val id = counter.getAndIncrement()

    override fun toString(): String {
        return "%$id"
    }
}

// Types
class Integer(val value: Int): Value(BasicType.IntType)
class Bool(val value: Boolean): Value(BasicType.BoolType)
class Character(val value: Char): Value(BasicType.CharType)

// Binary Operations TODO
class Compare(type: ReturnableType): Value(type)
class Add: Value(BasicType.IntType)
class Sub: Value(BasicType.IntType)
class Mult: Value(BasicType.IntType)
class Div: Value(BasicType.IntType)
class Assign(val value: Value): Instruction {
    val register = Register.generate(value.type)

    override fun toString(): String {
        return "$register = $value"
    }
}

// Memory Operations
class Allocate(type: ReturnableType, val elemCount: Int): Instruction {
    constructor(type: ReturnableType): this(type, 1)

    val register: Register = Register.generate(type)

    override fun toString(): String {
        return "<${register.type}> allocate ${if (elemCount != 1) {"[$elemCount]"} else {""} }"
    }
}
class Load(val address: Register): Value(address.type) {
    override fun toString(): String {
        return "<${address.type}> load <${address.type}*> [$address]"
    }
}
class Store(val value: Value, val address: Register): Instruction {
    override fun toString(): String {
        return "store <${value.type}> $value <${address.type}*> [$address]"
    }
}

// Other Operations
class Return(val value: Value): Instruction {
    override fun toString(): String {
        return "return $value"
    }
}

class CondBranch(val value: Value, val branchTrue: Block, val branchFalse: Block): Instruction {
    override fun toString(): String {
        return "branch if $value %$branchTrue else %$branchFalse"
    }
}

class Branch(val branch: Block): Instruction {
    override fun toString(): String {
        return "branch %$branch"
    }
}
class Parameter(type: ReturnableType): Value(type) {
    val register: Register = Register.generate(type)
}
class Phi(type: ReturnableType): Value(type) {
    private val operands = mutableListOf<Pair<Value, Block>>()

    fun addOperand(vararg operands: Pair<Value, Block>) {
        this.operands.addAll(operands)
    }

    override fun toString(): String {
        val sb = StringBuilder("<$type> phi ")
        val wrap = operands.map { op -> "[${op.first} from ${op.second}]" }
        sb.append(wrap.joinToString(separator = ", "))
        return sb.toString()
    }
}
class Call(val function: Function, vararg args: Parameter): Value(function.type) {
    val arguments: List<Parameter> = args.asList()

    init {
        if (args.size != function.arguments.size) {
            throw IllegalArgumentException("Argument count passed in does not match function argument count")
        }
    }
    override fun toString(): String {
        val sb = StringBuilder("<${function.type} call @$function ")
        sb.append(arguments.joinToString(separator = ", ", prefix = "(", postfix = ")"))
        return sb.toString()
    }
}
class AccessArrayElem(type: ReturnableType): Value(type)
class AccessPairElem(type: ReturnableType): Value(type)

// Function
class Function(type: ReturnableType, val name: String, val arguments: Collection<Parameter>, val entryBlock: Block, val blocks: Collection<Block>): Value(type)

// Basic block
// Phi instructions must precede other instructions
class Block {
    companion object {
        private val labelGenerator = UniqueLabelGenerator("B")
    }

    private val label = labelGenerator.generate()
    private val instructions = mutableListOf<Instruction>()
    private val predecessors = mutableListOf<Block>()
    private lateinit var successor: Block

    override fun toString(): String {
        return label
    }

    fun addInstructions(vararg instructions: Instruction) {
        this.instructions.addAll(instructions)
    }

    fun addPredecessors(vararg predecessors: Block) {
        this.predecessors.addAll(predecessors)
    }

    fun setSuccessor(successor: Block) {
        this.successor = successor
    }
}