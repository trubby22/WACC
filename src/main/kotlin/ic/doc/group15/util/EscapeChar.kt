package ic.doc.group15.util

enum class EscapeChar(
    val letter: Char,
    val char: Char,
    val string: String
) {

    NUL('0', '\u0000', "\\0"),
    BS('b', '\b', "\\b"),
    TAB('t', '\t', "\\t"),
    LF('n', '\n', "\\n"),
    FF('f', '\u000c', "\\f"),
    CR('r', '\r', "\\r"),
    DQ('"', '\"', "\""),
    SQ('\'', '\'', "\'"),
    SL('\\', '\\', "\\")
    ;

    companion object {

        private val letterMap: Map<Char, EscapeChar> by lazy {
            values().map { Pair(it.letter, it) }.toMap()
        }

        private val charMap: Map<Char, EscapeChar> by lazy {
            values().map { Pair(it.char, it) }.toMap()
        }

        fun fromLetter(letter: Char): EscapeChar? {
            return letterMap[letter]
        }

        fun fromChar(char: Char): EscapeChar? {
            return charMap[char]
        }
    }

    override fun toString(): String {
        return string
    }
}
