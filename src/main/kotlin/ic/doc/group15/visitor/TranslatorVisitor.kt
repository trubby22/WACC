package ic.doc.group15.visitor

import kotlin.reflect.KCallable
import kotlin.reflect.KClass
import kotlin.reflect.jvm.isAccessible

private typealias VisitorClass = KClass<out TranslatorVisitor<*>>
private typealias VisitorMethodMap = Map<KClass<*>, KCallable<*>>

interface Translatable

/**
 * Generic visitor class.
 *
 * Uses the annotation [TranslatorMethod] to determine which method is used to visit a particular type.
 */
abstract class TranslatorVisitor<T : Any> protected constructor(enableLogging: Boolean = false) {

    private val klass: VisitorClass
    private val enableLogging: Boolean

    init {
        klass = this::class
        this.enableLogging = enableLogging
    }

    /**
     * Visits an item by calling the method responsible for visiting its type.
     */
    protected fun translate(item: T, vararg params: Any?) {
        val itemClass = item::class
        val translator = getMethodMap(klass)[itemClass]
        translator?.call(this, item, *params)
    }

    private companion object {
        val visitorMap: MutableMap<VisitorClass, VisitorMethodMap> = HashMap()

        fun getMethodMap(klass: VisitorClass): VisitorMethodMap {
            if (!visitorMap.contains(klass)) {
                visitorMap[klass] = generateMethodMap(klass)
            }
            return visitorMap[klass]!!
        }

        fun generateMethodMap(visitorClass: VisitorClass): VisitorMethodMap {
            return visitorClass.members.filter {
                it.annotations.isNotEmpty() && it.annotations.all { a -> a is TranslatorMethod }
            }.associateBy {
                assert(it.annotations.size == 1)
                it.isAccessible = true
                val klass = it.parameters[1].type.classifier
                assert(klass is KClass<*>)
                klass as KClass<*>
            }
        }
    }

    /**
     * Prints a log message to standard output if logging is enabled.
     */
    protected fun log(str: String) {
        if (enableLogging) {
            println(str)
        }
    }
}
