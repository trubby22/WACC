package ic.doc.group15.visitor

import ic.doc.group15.assembly.TranslatorMethod
import ic.doc.group15.ast.AST
import ic.doc.group15.ast.ASTNode
import ic.doc.group15.ast.BlockAST
import kotlin.reflect.KCallable
import kotlin.reflect.KClass
import kotlin.reflect.full.companionObjectInstance
import kotlin.reflect.jvm.isAccessible

private typealias TranslatorMap = Map<KClass<out ASTNode>, KCallable<*>>

abstract class ASTVisitor(
    private val ast: AST,
    private val enableLogging: Boolean = true
) {

    private val translators: TranslatorMap by lazy {
        this::class.members
            .filter {
                it.annotations.isNotEmpty() && it.annotations.all { a -> a is TranslatorMethod }
            }.map {
                assert(it.annotations.size == 1)
                it.isAccessible = true
                val annotation = it.annotations[0] as TranslatorMethod
                annotation.nodeType to it
            }.toMap()
    }

    protected fun translate(node: ASTNode) {
//        log("translators.keys: ${translators.keys}")
//        log("${this::class.simpleName}")
        translators[node::class]?.call(this, node)
    }

    protected fun translate(node: ASTNode, scope: BlockAST) {
        translators[node::class]?.call(this, node, scope)
    }

    protected fun log(str: String) {
        if (enableLogging) {
            println(str)
        }
    }
}