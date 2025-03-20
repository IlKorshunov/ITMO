@file:Suppress("DuplicatedCode", "FoldInitializerAndIfToElvis")

import java.util.concurrent.atomic.*

class MSQueueWithConstantTimeRemove<E> : QueueWithRemove<E> {
    private val head: AtomicReference<Node<E>>
    private val tail: AtomicReference<Node<E>>

    init {
        val dummy = Node<E>(element = null, prev = null)
        head = AtomicReference(dummy)
        tail = AtomicReference(dummy)
    }

    private fun setPrev(node: Node<E>, prev: Node<E>?) {
        node.prev.set(prev)
    }


    override fun enqueue(element: E) {
        val newNode = Node(element, null)
        while (true) {
            val curTail = tail.get()
            setPrev(newNode, curTail)
            if (curTail.next.compareAndSet(null, newNode)) {
                tail.compareAndSet(curTail, newNode)
                if (curTail.extractedOrRemoved) curTail.remove()
                return
            } else {
                tail.compareAndSet(curTail, curTail.next.get())
            }
        }
    }

    override fun dequeue(): E? {
        while (true) {
            val curHead = head.get()
            val curHeadNext = curHead.next.get() ?: return null
            val out = curHeadNext.element
            if (head.compareAndSet(curHead, curHeadNext)) {
                curHeadNext.element = null
                setPrev(curHeadNext, null)
                if (!curHeadNext.markExtractedOrRemoved()) continue
                return out
            }
        }
    }

    override fun remove(element: E): Boolean {
        var node = head.get()
        while (true) {
            val next = node.next.get()
            if (next == null) return false
            node = next
            if (node.element == element && node.remove()) return true
        }
    }

    override fun validate() {
        check(head.get().prev.get() == null) {
            "`head.prev` must be null"
        }
        check(tail.get().next.get() == null) {
            "tail.next must be null"
        }
        var node = head.get()
        while (true) {
            if (node !== head.get() && node !== tail.get()) {
                check(!node.extractedOrRemoved) {
                    "Removed node with element ${node.element} found in the middle of the queue"
                }
            }
            val nodeNext = node.next.get()
            if (nodeNext == null) break
            val nodeNextPrev = nodeNext.prev.get()
            check(nodeNextPrev != null) {
                "The `prev` pointer of node with element ${nodeNext.element} is `null`, while the node is in the middle of the queue"
            }
            check(nodeNextPrev == node) {
                "node.next.prev != node; `node` contains ${node.element}, `node.next` contains ${nodeNext.element}"
            }
            node = nodeNext
        }
    }

    private class Node<E>(
        var element: E?,
        prev: Node<E>?
    ) {
        val next = AtomicReference<Node<E>?>(null)
        val prev = AtomicReference(prev)

        private val _extractedOrRemoved = AtomicBoolean(false)
        val extractedOrRemoved
            get() =
                _extractedOrRemoved.get()

        fun markExtractedOrRemoved(): Boolean =
            _extractedOrRemoved.compareAndSet(false, true)

        fun remove(): Boolean {
            val flag : Boolean = markExtractedOrRemoved()
            val next = next.get()
            val prev = prev.get()
            if (prev == null || next == null) return flag
            prev.next.compareAndSet(this, next)
            next.prev.compareAndSet(this, prev)
            if (prev.extractedOrRemoved) prev.remove()
            if (next.extractedOrRemoved) next.remove()
            return flag
        }
    }
}