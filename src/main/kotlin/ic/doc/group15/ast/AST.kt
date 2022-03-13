package ic.doc.group15.ast

import ic.doc.group15.SymbolTable
import ic.doc.group15.assembly.TranslatorMethod
import kotlin.reflect.KCallable
import kotlin.reflect.KClass
import kotlin.reflect.jvm.isAccessible

abstract class ASTNode protected constructor(
    val symbolTable: SymbolTable = SymbolTable.emptyTable
)

class AST(topLevelSymbolTable: SymbolTable) : BeginEndBlockAST(null, topLevelSymbolTable)

typealias ASTTranslatorMap = Map<KClass<out ASTNode>, KCallable<*>>

fun generateASTTranslatorMap(klass: KClass<*>): ASTTranslatorMap {
    return klass.members.filter {
        it.annotations.isNotEmpty() && it.annotations.all { a -> a is TranslatorMethod }
    }.associateBy {
        assert(it.annotations.size == 1)
        it.isAccessible = true
        val annotation = it.annotations[0] as TranslatorMethod
        annotation.nodeType
    }
}