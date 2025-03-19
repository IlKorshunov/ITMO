package httpgauge

import (
	"fmt"
	"net/http"
	"sort"
	"sync"

	"github.com/go-chi/chi/v5"
)

//

type Gauge struct {
	mu     sync.Mutex
	forAns map[string]int
}

func New() *Gauge {
	return &Gauge{
		forAns: make(map[string]int),
	}
}

func (g *Gauge) Snapshot() map[string]int {
	g.mu.Lock()
	defer g.mu.Unlock()
	return g.forAns
}

func (g *Gauge) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	g.mu.Lock()
	defer g.mu.Unlock()
	keys := make([]string, 0, len(g.forAns))
	for key := range g.forAns {
		keys = append(keys, key)
	}
	sort.Strings(keys)
	sortedMap := make(map[string]int, len(keys))
	for _, key := range keys {
		sortedMap[key] = g.forAns[key]
	}

	for pattern, count := range sortedMap {
		fmt.Fprintf(w, "%s %d\n", pattern, count)
	}
}

func (g *Gauge) Wrap(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		defer func() {
			if err := recover(); err != nil {
				pattern := "/panic"
				g.forAns[pattern]++
				panic("bug")
			}
		}()
		next.ServeHTTP(w, r)
		pat := chi.RouteContext(r.Context()).RoutePattern()
		g.mu.Lock()
		g.forAns[pat]++
		g.mu.Unlock()
	})
}
