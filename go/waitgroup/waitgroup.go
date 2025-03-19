package waitgroup

type WaitGroup struct {
	count int
	mutex chan struct{}
	done  chan struct{}
}

func New() *WaitGroup {
	return &WaitGroup{
		count: 0,
		mutex: make(chan struct{}, 1),
		done:  make(chan struct{}),
	}
}

func (wg *WaitGroup) Add(delta int) {
	wg.mutex <- struct{}{}
	wg.count += delta
	if wg.count < 0 {
		<-wg.mutex
		panic("negative WaitGroup counter")
	}

	if wg.count == 0 {
		close(wg.done)
		wg.done = make(chan struct{})
	}
	<-wg.mutex
}

func (wg *WaitGroup) Done() {
	//wg.mutex <- struct{}{}
	wg.Add(-1)
	//<-wg.mutex
}

func (wg *WaitGroup) Wait() {
	wg.mutex <- struct{}{}
	if wg.count == 0 {
		<-wg.mutex
		return
	}
	done := wg.done
	<-wg.mutex
	<-done
}
