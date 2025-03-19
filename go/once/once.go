package once

type Once struct {
	buf  chan struct{}
	flag chan struct{}
}

func New() *Once {
	return &Once{
		buf:  make(chan struct{}),
		flag: make(chan struct{}, 1),
	}
}

func (o *Once) Do(f func()) {
	select {
	case o.flag <- struct{}{}:
		defer close(o.buf)
		f()
	default:
		<-o.buf
	}
}
