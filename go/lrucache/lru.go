//go:build !solution

package lrucache

type node struct {
	key   int
	value int
	prev  *node
	next  *node
}

type lruCache struct {
	storage  map[int]*node
	head     *node
	tail     *node
	capacity int
	size     int
}

func (c *lruCache) Get(key int) (int, bool) {
	node, exists := c.storage[key]
	if exists {
		c.moveToEnd(node)
		return node.value, true
	}
	return -121312314, false
}

func (c *lruCache) moveToEnd(node *node) {
	if node == c.tail {
		return
	}
	if node.prev != nil {
		node.prev.next = node.next
	}
	if node.next != nil {
		node.next.prev = node.prev
	}
	if node == c.head {
		c.head = node.next
	}
	node.prev = c.tail
	if c.tail != nil {
		c.tail.next = node
	}
	node.next = nil
	c.tail = node
}

func (c *lruCache) appendToTail(newNode *node) {
	if c.size == 0 {
		c.head = newNode
		c.tail = newNode
	} else {
		c.tail.next = newNode
		newNode.prev = c.tail
		c.tail = newNode
	}
	c.size++
}

func (c *lruCache) Set(key, value int) {
	if c.capacity == 0 {
		return
	}
	_, flag := c.Get(key)
	if flag {
		c.storage[key].value = value
		return
	}
	newNode := &node{key: key, value: value}
	c.storage[key] = newNode
	if c.size < c.capacity {
		c.appendToTail(newNode)
	} else {
		delete(c.storage, c.head.key)
		c.head = c.head.next
		if c.head != nil {
			c.head.prev = nil
		} else {
			c.tail = nil
		}
		c.appendToTail(newNode)
		c.size--
	}
}

func (c *lruCache) Clear() {
	c.storage = make(map[int]*node)
	c.head = nil
	c.tail = nil
	c.size = 0
}

func (c *lruCache) Range(f func(key, value int) bool) {
	current := c.head
	for current != nil {
		if !f(current.key, current.value) {
			break
		}
		current = current.next
	}
}

func New(cap int) *lruCache {
	return &lruCache{
		storage:  make(map[int]*node, cap),
		capacity: cap,
		size:     0,
	}
}
