package ic.doc.group15

typealias ArgsList = Array<String>

interface Arg

interface OptionArg : Arg {
    fun string(): String
    fun alt(): String
}

fun ArgsList.hasOption(option: OptionArg): Boolean {
    if (contains("--" + option.string())) return true
    return contains("-" + option.alt())
}

enum class Option(val alt: String, val description: String) : OptionArg {

    HELP(
        "H",
        "Displays the help message."
    ),
    LOGGING(
        "L",
        "Enables all logging to standard output, including AST generation logs and " +
            "codegen logs."
    ),
    PRINT_ASM(
        "P",
        "Prints the generated assembly code to standard output."
    ),
    ;

    override fun string(): String {
        return name.lowercase().replace("_", "-")
    }

    override fun alt(): String {
        return alt
    }
}
