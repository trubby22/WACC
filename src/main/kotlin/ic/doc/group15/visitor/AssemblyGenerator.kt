package ic.doc.group15.visitor

import ic.doc.group15.assembly.*
import ic.doc.group15.assembly.operand.BranchLabelOperand
import java.io.BufferedWriter

private fun BufferedWriter.writeAsm(vararg labels: Collection<Label<*>>) {
    labels.forEach {
        it.forEach {
                label ->
            write(label.toString())
            write("\n")
        }
    }
}

/**
 * Generic assembly generator.
 *
 * Accepts a start item of type T, assumed to be some sort of tree data structure, and visits each
 * node, translating it into ARM assembly.
 */
abstract class AssemblyGenerator<T : Any> protected constructor(
    private val start: T,
    enableLogging: Boolean
) : TranslatorVisitor<T>(enableLogging) {

    /**
     * The current label that the generator is adding instructions to as it translates them.
     */
    protected lateinit var currentLabel: BranchLabel

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
    protected val text: MutableMap<String, BranchLabel> = mutableMapOf()
    private val utilText: MutableMap<String, BranchLabel> = mutableMapOf()

    private val stringLabelGenerator = UniqueStringLabelGenerator()
    private val branchLabelGenerator = UniqueBranchLabelGenerator()

    /**
     * Generates the assembly lines and writes them to the provided writer.
     */
    fun generate(writer: BufferedWriter) {
        log("Generating assembly...")
        generateAssembly(start)

        log("Writing .data section")
        if (data.isNotEmpty() || utilData.isNotEmpty()) {
            writer.write(".data\n\n")
            writer.writeAsm(data.values, utilData.values)
        }

        log("Writing .text section")
        writer.write("\n.text\n\n.global main\n")
        writer.writeAsm(text.values, utilText.values)
    }

    /**
     * Extend this method to determine how the generator translates to assembly.
     */
    protected fun generateAssembly(startItem: T) {
        translate(startItem)
    }

    /**
     * Creates a new data label storing a string and adds it to the data section.
     */
    protected fun newStringLabel(str: String): StringData {
        return newStringLabel(stringLabelGenerator.generate(), str)
    }
    protected fun newStringLabel(name: String, str: String): StringData {
        log("Generating string label: $name")
        val label = StringData(name, str)
        data[label.name] = label
        return label
    }

    /**
     * Creates a new instruction label and adds it to the text section.
     */
    protected fun newBranchLabel(
        addToText: Boolean = true,
        vararg lines: Instruction
    ): BranchLabel {
        return newBranchLabel(branchLabelGenerator.generate(), addToText, *lines)
    }

    protected fun newBranchLabel(
        name: String,
        addToText: Boolean = true,
        vararg lines: Instruction
    ): BranchLabel {
        log("Generating branch label: $name")
        val label = BranchLabel(name, *lines)
        if (addToText) {
            addToText(label)
        }
        return label
    }

    protected fun addToText(vararg label: BranchLabel) {
        label.forEach { text[it.name] = it }
    }

    /**
     * Creates a new instruction label for a custom function and adds it to the text section.
     */
    protected fun newFunctionLabel(funcName: String, vararg lines: Instruction): BranchLabel {
        return newBranchLabel("f_$funcName", true, *lines)
    }
    protected fun branchToFunction(funcName: String): BranchLabelOperand {
        return BranchLabelOperand("f_$funcName")
    }

    /**
     * Add the provided assembly instructions to the current label.
     */
    protected fun addLines(vararg lines: Instruction) {
        currentLabel.addLines(*lines)
    }

    protected fun addLines(lines: Collection<Instruction>) {
        currentLabel.addLines(lines)
    }

    /**
     * Adds a utility function to the generated assembly file.
     */
    protected fun defineUtilFuncs(vararg funcs: UtilFunction) {
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
}
