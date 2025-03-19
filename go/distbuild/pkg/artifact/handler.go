//go:build !solution

package artifact

import (
	"net/http"

	"gitlab.com/slon/shad-go/distbuild/pkg/build"
	"gitlab.com/slon/shad-go/distbuild/pkg/tarstream"
	"go.uber.org/zap"
)

type Handler struct {
	myCache *Cache
	myLogger *zap.Logger
}

func NewHandler(l *zap.Logger, c *Cache) *Handler {
	return &Handler{
		myCache: c,
		myLogger: l,
	}
}

func (h *Handler) Register(mux *http.ServeMux) {
	mux.HandleFunc("/artifact", h.download)
}

func (h *Handler) download(w http.ResponseWriter, r *http.Request) {
    h.myLogger.Info("Start download in artifact handler")
    idStr := r.URL.Query().Get("id")
    if idStr == "" {
        h.myLogger.Info("Error in missing id parametr")
        http.Error(w, "Missing id parameter", http.StatusBadRequest)
        return
    }

    var id build.ID
    if err := id.UnmarshalText([]byte(idStr)); err != nil {
        h.myLogger.Info("Invalid id in artifact")
        http.Error(w, "Invalid ID format", http.StatusBadRequest)
        return
    }

    path, unlock, err := h.myCache.Get(id)
    if err != nil {
        if err == ErrNotFound {
            h.myLogger.Info("Artifact not found")
            http.Error(w, "Artifact not found", http.StatusNotFound)
        } else {
            h.myLogger.Error("got unkown error in artifact handler", zap.Error(err))
            http.Error(w, err.Error(), http.StatusInternalServerError)
        }
        return
    }
    defer unlock()

    w.Header().Set("Content-Type", "application/octet-stream")
    if err := tarstream.Send(path, w); err != nil {
        h.myLogger.Error("Failed to send artifact", zap.Error(err))
    }
}
