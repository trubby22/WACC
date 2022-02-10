package ic.doc.group15.semantics

class SymbolTable private constructor(private val enclosingTable: SymbolTable?) {

    private val map: MutableMap<String, Identifier> = HashMap()

    companion object {

        val emptyTable: SymbolTable = SymbolTable(null)

        private val basicTypes = mapOf(
            Pair("int", BasicType.IntType),
            Pair("bool", BasicType.BoolType),
            Pair("char", BasicType.CharType),
            Pair("string", BasicType.StringType),
        )

        fun topLevel(): SymbolTable {
            val st = SymbolTable(null)
            st.map.putAll(basicTypes)
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
            val ident = st.lookup(name)
            if (ident != null) return ident
            st = st.enclosingTable
        }
        return null
    }

    fun subScope(): SymbolTable {
        return SymbolTable(this)
    }

    fun parentScope(): SymbolTable? {
        return enclosingTable
    }
}
