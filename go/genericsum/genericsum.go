//go:build !solution

package genericsum

import (
	"math/cmplx"
	"sort"
	"sync"

	"golang.org/x/exp/constraints"
)

func Min[T constraints.Ordered](x, y T) T {
	if x < y {
		return x
	}
	return y
}

type orderedSlice[T constraints.Ordered] []T

func (s orderedSlice[T]) Len() int           { return len(s) }
func (s orderedSlice[T]) Less(i, j int) bool { return s[i] < s[j] }
func (s orderedSlice[T]) Swap(i, j int)      { s[i], s[j] = s[j], s[i] }

func SortSlice[T constraints.Ordered](s []T) {
	sort.Sort(orderedSlice[T](s))
}

// func SortSlice[T constraints.Ordered](s []T) {
// 	sort.Slice(s, func(i, j int) bool {
// 		return s[i] < s[j]
// 	})
// }

func MapsEqual[T comparable, U comparable](a, b map[T]U) bool {
	if len(a) != len(b) {
		return false
	}
	for k1, v1 := range a {
		v2, flag := b[k1]
		if !flag || v2 != v1 {
			return false
		}
	}
	return true
}

func SliceContains[T comparable](s []T, v T) bool {
	for _, curValue := range s {
		if curValue == v {
			return true
		}
	}
	return false
}

func MergeChans[T any](chans ...<-chan T) <-chan T {
	var wg sync.WaitGroup
	out := make(chan T)
	wg.Add(len(chans))

	localFunc := func(c <-chan T) {
		for val := range c {
			out <- val
		}
		wg.Done()
	}

	for _, cur := range chans {
		go localFunc(cur)
	}

	go func() {
		wg.Wait()
		close(out)
	}()

	return out
}

func IsHermitianMatrix[T int | complex128 | complex64](m [][]T) bool {
	n := len(m)

	for i := 0; i < n; i++ {
		if len(m[i]) != n {
			return false
		}
	}

	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			switch v := any(m[i][j]).(type) {
			case int:
				value, ok := any(m[j][i]).(int)
				if !ok || v != value {
					return false
				}
			case complex64:
				value, ok := any(m[j][i]).(complex64)
				if !ok || v != complex64(cmplx.Conj(complex128(value))) {
					return false
				}
			case complex128:
				value, ok := any(m[j][i]).(complex128)
				if !ok || v != cmplx.Conj(value) {
					return false
				}
			}
		}
	}

	return true
}
