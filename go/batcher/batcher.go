package batcher

import (
	"gitlab.com/slon/shad-go/batcher/slow"
	"sync"
	"time"
)

type Batcher struct {
	value          *slow.Value
	mu             sync.Mutex
	ready          chan struct{}
	loadInProgress bool
	res            interface{}
	version        int
	cond           *sync.Cond
	time           time.Time
}

func NewBatcher(v *slow.Value) *Batcher {
	b := &Batcher{
		value:          v,
		ready:          make(chan struct{}),
		loadInProgress: false,
		version:        0,
		time:           time.Now(),
	}
	b.cond = sync.NewCond(&b.mu)
	return b
}

func (b *Batcher) Load() interface{} {
	enter := time.Now()

	b.mu.Lock()
	for enter.After(b.time) {
		if !b.loadInProgress {
			b.loadInProgress = true
			b.mu.Unlock()

			start := time.Now()
			temp := b.value.Load()

			b.mu.Lock()
			b.time = start
			b.res = temp
			b.loadInProgress = false
			b.mu.Unlock()
			b.cond.Broadcast()
			return temp
		}
		b.cond.Wait()
	}

	t := b.res
	b.mu.Unlock()
	return t
}
