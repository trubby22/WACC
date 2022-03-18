package ic.doc.group15.util

import java.util.concurrent.atomic.AtomicInteger

abstract class UniqueGenerator<T : Any> protected constructor() {

    private val counter = AtomicInteger(0)

    protected fun uniqueNum() = counter.getAndIncrement()

    abstract fun generate(): T
}
