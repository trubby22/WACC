package ic.doc.group15.visitor

import kotlin.reflect.KCallable
import kotlin.reflect.KClass
import kotlin.reflect.jvm.isAccessible

private typealias VisitorClass = KClass<out Visitor<*>>
private typealias VisitorMethodMap = Map<KClass<*>, KCallable<*>>

interface Visitable

/**
 * Generic visitor class.
 *
 * Uses the annotation [VisitorMethod] to determine which method is used to visit a particular type.
 */
abstract class Visitor<T : Any> protected constructor() {

    private val klass: VisitorClass

    init {
        klass = this::class
    }

    /**
     * Visits an item by calling the method responsible for visiting its type.
     */
    protected fun visit(item: T) {
        getMethodMap(klass)[item::class]?.call(this, item)
    }

    private companion object {
        val visitorMap: MutableMap<VisitorClass, VisitorMethodMap> = HashMap()

        fun getMethodMap(klass: VisitorClass): VisitorMethodMap {
            if (!visitorMap.contains(klass)) {
                visitorMap[klass] = generateMethodMap(klass)
            }
            return visitorMap[klass]!!
        }

        fun generateMethodMap(klass: VisitorClass): VisitorMethodMap {
            return klass.members.filter {
                it.annotations.isNotEmpty() && it.annotations.all { a -> a is VisitorMethod }
            }.associateBy {
                assert(it.annotations.size == 1)
                it.isAccessible = true
                val annotation = it.annotations[0] as VisitorMethod
                annotation.itemType
            }
        }
    }
}
