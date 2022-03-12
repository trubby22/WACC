package ic.doc.group15.assembly

import ic.doc.group15.ast.ASTNode
import kotlin.reflect.KCallable
import kotlin.reflect.KClass

@Target(AnnotationTarget.FUNCTION)
annotation class TranslatorMethod(val nodeType: KClass<out ASTNode>)