package dupcall

import (
	"context"
	"sync"
)

type Call struct {
	myMutex   sync.Mutex
	res       interface{}
	err       error
	myChannel chan struct{}
}

func (o *Call) Do(
	ctx context.Context,
	cb func(context.Context) (interface{}, error),
) (res interface{}, err error) {
	o.myMutex.Lock()
	if o.myChannel == nil {
		o.myChannel = make(chan struct{})
		go o.run(ctx, cb)
	}
	localChannel := o.myChannel
	o.myMutex.Unlock()
	select {
	case <-ctx.Done():
		return nil, ctx.Err()
	case <-localChannel:
		o.myMutex.Lock()
		defer o.myMutex.Unlock()
		return o.res, o.err
	}
}

func (o *Call) run(ctx context.Context, cb func(context.Context) (interface{}, error)) {
	o.res, o.err = cb(ctx)
	o.myMutex.Lock()
	close(o.myChannel)
	o.myChannel = nil
	o.myMutex.Unlock()
}
