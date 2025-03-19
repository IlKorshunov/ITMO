//go:build !solution

package requestlog

import (
	"fmt"
	"net/http"
	"time"

	"go.uber.org/zap"
)

func Log(l *zap.Logger) func(next http.Handler) http.Handler {
	return func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			start := time.Now()
			ID := gentID()
			temp := NewTemp(w)
			l.Info("request started",
				zap.String("path", r.URL.Path),
				zap.String("request_id", ID),
				zap.String("method", r.Method),
			)

			defer func() {
				dur := time.Since(start)

				if p := recover(); p != nil {
					l.Error("request panicked",
						zap.String("path", r.URL.Path),
						zap.String("method", r.Method),
						zap.String("request_id", ID),
						zap.Duration("duration", dur),
					)
					panic(p)
				} else {
					l.Info("request finished",
						zap.String("path", r.URL.Path),
						zap.String("method", r.Method),
						zap.String("request_id", ID),
						zap.Duration("duration", dur),
						zap.Int("status_code", temp.out),
					)
				}
			}()

			next.ServeHTTP(temp, r)
		})
	}
}

// func logRequest(l *zap.Logger, r *http.Request, start time.Time, ID string, statusCode int) {
// 	dur := time.Since(start)

// 	if p := recover(); p != nil {
// 		l.Error("request panicked",
// 			zap.String("path", r.URL.Path),
// 			zap.String("method", r.Method),
// 			zap.String("request_id", ID),
// 			zap.Duration("duration", dur),
// 		)
// 		panic(p)
// 	} else {
// 		l.Info("request finished",
// 			zap.String("path", r.URL.Path),
// 			zap.String("method", r.Method),
// 			zap.String("request_id", ID),
// 			zap.Duration("duration", dur),
// 			zap.Int("status_code", statusCode),
// 		)
// 	}
// }

func logRequest(l *zap.Logger, r *http.Request, start time.Time, ID string, statusCode int) {
	dur := time.Since(start)

	if p := recover(); p != nil {
		l.Error("request panicked",
			zap.String("path", r.URL.Path),
			zap.String("method", r.Method),
			zap.String("request_id", ID),
			zap.Duration("duration", dur),
		)
		panic(p)
	} else {
		l.Info("request finished",
			zap.String("path", r.URL.Path),
			zap.String("method", r.Method),
			zap.String("request_id", ID),
			zap.Duration("duration", dur),
			zap.Int("status_code", statusCode),
		)
	}
}

//

type response struct {
	http.ResponseWriter
	out int
}

func (res *response) WriteHeader(code int) {
	res.out = code
	res.ResponseWriter.WriteHeader(code)
}

func NewTemp(w http.ResponseWriter) *response {
	return &response{w, http.StatusOK}
}

func gentID() string {
	now := time.Now()
	return fmt.Sprintf("%d%09d", now.Unix(), now.Nanosecond())
}
