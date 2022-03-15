package ic.doc.group15.translator

import ic.doc.group15.visitor.Translatable
import kotlin.reflect.KClass

@Target(AnnotationTarget.FUNCTION)
annotation class TranslatorMethod(val itemType: KClass<out Translatable>)
