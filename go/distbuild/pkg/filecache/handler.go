//go:build !solution

package filecache

import (
	"io"
	"net/http"
	"os"

	"gitlab.com/slon/shad-go/distbuild/pkg/build"
	"go.uber.org/zap"
)

type Handler struct {
	cache *Cache
	logger *zap.Logger
}

func NewHandler(l *zap.Logger, cache *Cache) *Handler {
	return &Handler{
		cache: cache,
		logger: l,
	}
}

func (h *Handler) Register(mux *http.ServeMux) {
    mux.HandleFunc("/file", func(w http.ResponseWriter, r *http.Request) {
        switch r.Method {
        case http.MethodGet:
            h.Download(w, r)
        case http.MethodPut:
            h.Upload(w, r)
        default:
            http.Error(w, "Unknown Method", http.StatusMethodNotAllowed)
        }
    })
}

func (h *Handler) Upload(w http.ResponseWriter, r *http.Request) {
    idStr := r.URL.Query().Get("id")
    if idStr == "" {
        http.Error(w, "Missing id parameter", http.StatusBadRequest)
        return
    }

    var id build.ID
    if err := id.UnmarshalText([]byte(idStr)); err != nil {
        http.Error(w, "Invalid ID format", http.StatusBadRequest)
        return
    }

    writer, abort, err := h.cache.Write(id)
    if err != nil {
        http.Error(w, err.Error(), http.StatusInternalServerError)
        return
    }

    if _, err := io.Copy(writer, r.Body); err != nil {
        abort()  
        http.Error(w, err.Error(), http.StatusInternalServerError)
        return
    }

    if err := writer.Close(); err != nil {
        abort()  
        http.Error(w, err.Error(), http.StatusInternalServerError)
        return
    }

    w.WriteHeader(http.StatusOK)
}



func (h *Handler) Download(w http.ResponseWriter, r *http.Request) {
    idStr := r.URL.Query().Get("id")
    if idStr == "" {
        http.Error(w, "Missing id parameter", http.StatusBadRequest)
        return
    }

    var id build.ID
    if err := id.UnmarshalText([]byte(idStr)); err != nil {
        http.Error(w, "Invalid ID format", http.StatusBadRequest)
        return
    }

    path, unlock, err := h.cache.Get(id)
    if err != nil {
        http.Error(w, err.Error(), http.StatusInternalServerError)
        return
    }
    defer unlock()

    file, err := os.Open(path)
    if err != nil {
        http.Error(w, err.Error(), http.StatusInternalServerError)
        return
    }
    defer file.Close()

    w.Header().Set("Content-Type", "application/octet-stream")
    if _, err := io.Copy(w, file); err != nil {
        http.Error(w, err.Error(), http.StatusInternalServerError)
        return
    }
}
