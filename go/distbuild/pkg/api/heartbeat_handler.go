//go:build !solution

package api

import (
	"encoding/json"
	"fmt"
	"net/http"

	"go.uber.org/zap"
)

type HeartbeatHandler struct {
	logger  *zap.Logger
	service HeartbeatService
}

func NewHeartbeatHandler(l *zap.Logger, s HeartbeatService) *HeartbeatHandler {
	return &HeartbeatHandler{
		logger: l,
		service: s,
	}
}

func (h *HeartbeatHandler) Register(mux *http.ServeMux) {
	mux.HandleFunc("/heartbeat", func(w http.ResponseWriter, r *http.Request) {
		//h.logger.Info("start process heartbeat")

		var request HeartbeatRequest
		decoder := json.NewDecoder(r.Body)
		err := decoder.Decode(&request)
		if err != nil {
			h.logger.Error("Failed to decode heartbeat", zap.Error(err))
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}

		response, err := h.service.Heartbeat(r.Context(), &request)
		if err != nil {
			h.logger.Error("Failed to process signal", zap.Error(err))
			http.Error(w, fmt.Sprintf(`{"error": "%s"}`, err.Error()), http.StatusInternalServerError)
			return
		}
		
		if err := json.NewEncoder(w).Encode(response); err != nil {
			h.logger.Error("Failed to encode response", zap.Error(err))
			http.Error(w, err.Error(), http.StatusInternalServerError)
		}

	})
}
