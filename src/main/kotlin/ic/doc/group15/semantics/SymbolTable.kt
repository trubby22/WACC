package ic.doc.group15.semantics

class SymbolTable private constructor(private val enclosingTable: SymbolTable?) {

    private val map: MutableMap<String, Identifier> = HashMap()

    companion object {
        private val basicTypes = mapOf(
            Pair("int", IntType(min = Int.MIN_VALUE, max = Int.MAX_VALUE)),
            Pair("float", FloatType(min = Float.MIN_VALUE, max = Float.MAX_VALUE)),
            Pair("char", CharType(min = 0, max = 255)),
            Pair("string", StringType()),
            Pair("bool", BoolType(falseVal = 0, trueVal = 1)),
        )

        fun topLevel(): SymbolTable {
            val st = SymbolTable(null)
            st.map.putAll(basicTypes)
            val standardFunctions = arrayOf(
                Pair("print", FunctionType(null, listOf(Param(StringType())), st.subScope())),
                Pair("println", FunctionType(null, listOf(Param(StringType())), st.subScope())),
                Pair(
                    "exit",
                    FunctionType(
                        null,
                        listOf(Param(basicTypes["int"]!!)),
                        st.subScope()
                    )
                ),
                Pair(
                    "newpair",
                    FunctionType(
                        PairType(Type(), Type()),
                        listOf(Param(Type()), Param(Type())),
                        st.subScope()
                    )
                )
            )
            st.map.putAll(standardFunctions)
            return st
        }
    }

    fun isTopLevel(): Boolean = enclosingTable == null

    fun add(name: String, ident: Identifier) {
        map[name] = ident
    }

    fun lookup(name: String): Identifier? {
        return map[name]
    }

    fun lookupAll(name: String): Identifier? {
        var st: SymbolTable? = this
        while (st != null) {
            var ident = st.lookup(name)
            if (ident != null) return ident
            st = st.enclosingTable
        }
        return null
    }

    fun subScope(): SymbolTable {
        return SymbolTable(this)
    }
}
