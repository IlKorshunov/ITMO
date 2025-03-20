import java.util.concurrent.atomic.*
import kotlin.math.*

/**
 * @author Korshunov Ilya
 */
class FAABasedQueueSimplified<E> : Queue<E> {
    private val infiniteArray = AtomicReferenceArray<Any?>(64) // conceptually infinite array
    private val enqIdx = AtomicLong(0)
    private val deqIdx = AtomicLong(0)

    override fun enqueue(element: E) {
        while (true) {
            val i = enqIdx.getAndIncrement()
            if (infiniteArray.compareAndSet(i.toInt(), null, element)) return
        }
    }

    @Suppress("UNCHECKED_CAST")
    override fun dequeue(): E? {
        while (true) {
            if (shouldNotTry()) return null
            val i = deqIdx.getAndIncrement()
            if (infiniteArray.compareAndSet(i.toInt(), null, POISONED)) continue
            val out = infiniteArray.get(i.toInt())
            infiniteArray.set(i.toInt(), null)
            return out as E
        }
    }

    fun shouldNotTry(): Boolean {
        while (true) {
            val localDeq = deqIdx.get()
            val localEnq = enqIdx.get()
            if (localDeq == deqIdx.get()) return localDeq >= localEnq
        }
    }

    override fun validate() {
        for (i in 0 until min(deqIdx.get().toInt(), enqIdx.get().toInt())) {
            check(infiniteArray[i] == null || infiniteArray[i] == POISONED) {
                "`infiniteArray[$i]` must be `null` or `POISONED` with `deqIdx = ${deqIdx.get()}` at the end of the execution"
            }
        }
        for (i in max(deqIdx.get().toInt(), enqIdx.get().toInt()) until infiniteArray.length()) {
            check(infiniteArray[i] == null || infiniteArray[i] == POISONED) {
                "`infiniteArray[$i]` must be `null` or `POISONED` with `enqIdx = ${enqIdx.get()}` at the end of the execution"
            }
        }
    }
}

private val POISONED = Any()
