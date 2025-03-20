import java.util.concurrent.*
import java.util.concurrent.atomic.*

/**
 * @author : Korshunov Ilya
 */
open class TreiberStackWithElimination<E> : Stack<E> {
    private val stack = TreiberStack<E>()
    private val eliminationArray = AtomicReferenceArray<Any?>(ELIMINATION_ARRAY_SIZE)

    override fun push(element: E) {
        if (tryPushElimination(element)) return
        stack.push(element)
    }

    protected open fun tryPushElimination(element: E): Boolean {
        val ind = randomCellIndex()
        if (eliminationArray.compareAndSet(ind, CELL_STATE_EMPTY, element)) {
            repeat(ELIMINATION_WAIT_CYCLES) {
                if (eliminationArray.compareAndSet(ind, CELL_STATE_RETRIEVED, CELL_STATE_EMPTY)) {
                    return true
                }
            }
            if (eliminationArray.compareAndSet(ind, element, CELL_STATE_EMPTY)) {
                return false
            }
            return eliminationArray.compareAndSet(ind, CELL_STATE_RETRIEVED, CELL_STATE_EMPTY)
        }
        return false
    }

    private fun tryPopElimination(): E? {
        val ind = randomCellIndex()
        val curVal = eliminationArray.get(ind)
        if (curVal != CELL_STATE_EMPTY && curVal != CELL_STATE_RETRIEVED && eliminationArray.compareAndSet(ind, curVal, CELL_STATE_RETRIEVED)) {
            @Suppress("UNCHECKED_CAST")
            return curVal as E
        }
        return null
    }

    override fun pop(): E? = tryPopElimination() ?: stack.pop()

    private fun randomCellIndex(): Int =
        ThreadLocalRandom.current().nextInt(eliminationArray.length())

    companion object {
        private const val ELIMINATION_ARRAY_SIZE = 2 // Do not change!
        private const val ELIMINATION_WAIT_CYCLES = 1 // Do not change!

        private val CELL_STATE_EMPTY = null
        private val CELL_STATE_RETRIEVED = Any()
    }
}
