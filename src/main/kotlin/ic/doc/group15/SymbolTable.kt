package ic.doc.group15

import ic.doc.group15.type.* // ktlint-disable no-unused-imports
import ic.doc.group15.type.BasicType.*
import java.lang.IllegalArgumentException
import kotlin.reflect.KClass

class SymbolTable private constructor(private val enclosingTable: SymbolTable?) {

    private val map: MutableMap<String, Identifier> = HashMap()

    private var stackSize: Int = 0

    fun <T : Identifier> getValuesByType(klass: KClass<T>): List<T> {
        return map.values.filterIsInstance(klass.java)
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
    }

    constructor() : this(null)

    fun isTopLevel(): Boolean = enclosingTable == null

    fun add(name: String, ident: Identifier) {
        if (ident is Variable) {
            stackSize += ident.type.size()
        }
        map[name] = ident
    }

    fun lookup(name: String): Identifier? = map[name] ?: basicTypes[name]

    fun lookupAll(name: String): Identifier? {
        val basicType = basicTypes[name]
        if (basicType != null) {
            return basicType
        }
        var st: SymbolTable? = this
        while (st != null) {
            val ident = st.lookup(name)
            if (ident != null) return ident
            st = st.enclosingTable
        }
        return null
    }

    fun calcScopeOffset(variable: Variable): Int {
        var stackOffset = 0
        var st: SymbolTable? = this
        while (st != null) {
            if (st.map.containsValue(variable)) {
                return stackOffset
            }
            stackOffset += st.stackSize
            st = st.enclosingTable
        }
        throw IllegalArgumentException("variable not found")
    }

    fun subScope(): SymbolTable = SymbolTable(this)

    fun parentScope(): SymbolTable? = enclosingTable

    fun getStackSize(): Int = stackSize
}
