import java.util.concurrent.atomic.*

/**
 * @author Korshunov Ilya
 */
class FAABasedQueue<E> : Queue<E> {
    private val start = Segment(0)
    private val enqIdx = AtomicLong(0)
    private val deqIdx = AtomicLong(0)
    private val head = AtomicReference<Segment>(start)
    private val tail = AtomicReference<Segment>(start)

    override fun enqueue(element: E) {
        while (true) {
            val curTail = tail.get()
            val i = enqIdx.getAndIncrement()
            val segment = findSegment(curTail, i / SEGMENT_SIZE, tail)
            if (segment.cells.compareAndSet((i % SEGMENT_SIZE).toInt(), null, element)) return
        }
    }

    @Suppress("UNCHECKED_CAST")
    override fun dequeue(): E? {
        while (true) {
            if (shouldNotTry()) return null
            val curHead = head.get()
            val i = deqIdx.getAndIncrement()
            val segment = findSegment(curHead, i / SEGMENT_SIZE, head)
            val ind = (i % SEGMENT_SIZE).toInt()
            if (segment.cells.compareAndSet(ind, null, POISONED)) continue
            val out = segment.cells.get(ind)
            segment.cells.set(ind, null)
            return out as E
        }
    }

    private fun findSegment(start: Segment, to: Long, moveSeg: AtomicReference<Segment>): Segment {
        var startedSegment = start
        while (startedSegment.id < to) {
            if (startedSegment.next.get() == null && startedSegment.next.compareAndSet(null, Segment(startedSegment.id + 1))) continue
            startedSegment = startedSegment.next.get()!!
        }
        val curSeg = moveSeg.get()
        if (startedSegment.id > curSeg.id) {
            moveSeg.compareAndSet(curSeg, startedSegment)
        }
        return startedSegment
    }

    fun shouldNotTry(): Boolean {
        while (true) {
            val localDeq = deqIdx.get()
            val localEnq = enqIdx.get()
            if (localDeq == deqIdx.get()) return localDeq >= localEnq
        }
    }
}

private class Segment(val id: Long) {
    val next = AtomicReference<Segment?>(null)
    val cells = AtomicReferenceArray<Any?>(SEGMENT_SIZE)
}

private const val SEGMENT_SIZE = 2
private val POISONED = Any()