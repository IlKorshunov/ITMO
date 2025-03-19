package treeiter

type Node[T any] interface {
	comparable
	Left() T
	Right() T
}

func DoInOrder[T Node[T]](root T, f func(node T)) {
	var zero T
	if root == zero {
		return
	}
	DoInOrder(root.Left(), f)
	f(root)
	DoInOrder(root.Right(), f)
}
