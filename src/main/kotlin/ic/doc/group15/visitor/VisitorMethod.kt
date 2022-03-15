package ic.doc.group15.visitor

import kotlin.reflect.KClass

@Target(AnnotationTarget.FUNCTION)
annotation class VisitorMethod(val itemType: KClass<out Visitable>)
