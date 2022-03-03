package ic.doc.group15

import ic.doc.group15.type.* // ktlint-disable no-unused-imports
import ic.doc.group15.type.BasicType.*
import kotlin.reflect.KClass

class SymbolTable private constructor(private val enclosingTable: SymbolTable?) {

    private val map: MutableMap<String, Identifier> = HashMap()

    private var stackSize: Int = 0

    fun <T : Identifier> getValuesByType(klass: KClass<T>): List<T> {
        return map.values.filter { it::class == klass }.map { it as T }
    }

    companion object {

        val emptyTable: SymbolTable = SymbolTable(null)

        private val basicTypes = mapOf(
            Pair("int", IntType),
            Pair("bool", BoolType),
            Pair("char", CharType),
            Pair("string", StringType),
            Pair("pair", PairType())
        )

        // Create a top-level symbol table for AST initialisation
        fun topLevel(): SymbolTable {
            val st = SymbolTable(null)
            st.map.putAll(basicTypes)
            return st
        }
    }

    fun isTopLevel(): Boolean = enclosingTable == null

    fun add(name: String, ident: Identifier) {
        if (ident is Variable) {
            assert(ident.type is ReturnableType)
            stackSize += (ident.type as ReturnableType).size()
        }
        map[name] = ident
    }

    fun lookup(name: String): Identifier? = map[name]

    fun lookupAll(name: String): Identifier? {
        var st: SymbolTable? = this
        while (st != null) {
            val ident = st.lookup(name)
            if (ident != null) return ident
            st = st.enclosingTable
        }
        return null
    }

    fun subScope(): SymbolTable = SymbolTable(this)

    fun parentScope(): SymbolTable? = enclosingTable

    fun getStackSize(): Int = stackSize
}
