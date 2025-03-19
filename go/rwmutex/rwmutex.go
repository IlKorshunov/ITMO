package rwmutex

// A RWMutex is a reader/writer mutual exclusion lock.
// The lock can be held by an arbitrary number of readers or a single writer.
// The zero value for a RWMutex is an unlocked mutex.
//
// If a goroutine holds a RWMutex for reading and another goroutine might
// call Lock, no goroutine should expect to be able to acquire a read lock
// until the initial read lock is released. In particular, this prohibits
// recursive read locking. This is to ensure that the lock eventually becomes
// available; a blocked Lock call excludes new readers from acquiring the
// lock.
type RWMutex struct {
	mutex chan struct{}
	flag  chan struct{}
	count int
}

func New() *RWMutex {
	mutex := &RWMutex{
		mutex: make(chan struct{}, 2),
		flag:  make(chan struct{}, 2),
		count: 0,
	}
	mutex.mutex <- struct{}{}
	mutex.flag <- struct{}{}
	return mutex
}

// RUnlock undoes a single RLock call;
// it does not affect other simultaneous readers.
// It is a run-time error if rw is not locked for reading
// on entry to RUnlock.
func (rwMutex *RWMutex) Unlock() {
	rwMutex.mutex <- struct{}{}
}

func (rwMutex *RWMutex) Lock() {
	<-rwMutex.mutex
}

func (rwMutex *RWMutex) RLock() {
	<-rwMutex.flag
	if rwMutex.count == 0 {
		<-rwMutex.mutex
	}
	rwMutex.count++
	rwMutex.flag <- struct{}{}
}

func (rwMutex *RWMutex) RUnlock() {
	<-rwMutex.flag
	rwMutex.count--
	if rwMutex.count == 0 {
		rwMutex.mutex <- struct{}{}
	}
	rwMutex.flag <- struct{}{}
}
