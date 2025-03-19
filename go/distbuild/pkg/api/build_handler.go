//go:build !solution

package api

import (
	"encoding/json"
	"fmt"
	"net/http"

	"gitlab.com/slon/shad-go/distbuild/pkg/build"
	"go.uber.org/zap"
)

func NewBuildService(l *zap.Logger, s Service) *BuildHandler {
	return &BuildHandler{
		logger: l,
		service: s,
	}
}

type BuildHandler struct {
	logger  *zap.Logger
	service Service
}

func (h *BuildHandler) Register(mux *http.ServeMux) {
	mux.HandleFunc("/build", func(w http.ResponseWriter, r *http.Request) {
		h.logger.Info("Received build signal")

		var buildRequest BuildRequest
		decoder := json.NewDecoder(r.Body)
		err := decoder.Decode(&buildRequest)
		if err != nil {
			h.logger.Error("Failed to decode signal build", zap.Error(err))
			return
		}
		statusWriter := NewStatusWriter(w)
		err = h.service.StartBuild(r.Context(), &buildRequest, statusWriter)
		if err != nil {
			if len(buildRequest.Graph.Jobs) == 0 && len(buildRequest.Graph.SourceFiles) == 0 {
				http.Error(w, fmt.Sprintf(`{"error": "%s"}`, err.Error()), http.StatusInternalServerError)
			}
			statusWriter.Error(err)
			h.logger.Error("Failed to process signal", zap.Error(err))
			return
		}
	})


	mux.HandleFunc("/signal", func(w http.ResponseWriter, r *http.Request) {
		h.logger.Info("Received signal")
		buildIDStr := r.URL.Query().Get("build_id")
		if buildIDStr == "" {
			h.logger.Error("No build ID provided")
			http.Error(w, "No build ID provided", http.StatusBadRequest)
			return
		}
	
		var signalReq SignalRequest
		decoder := json.NewDecoder(r.Body)
		if err := decoder.Decode(&signalReq); err != nil {
			h.logger.Error("Failed to decode signal request", zap.Error(err))
			return
		}
	
		var buildID build.ID
		err := buildID.UnmarshalText([]byte(buildIDStr))
		if err != nil {
			h.logger.Error("Invalid build ID format", zap.Error(err))
			http.Error(w, "Invalid build ID format", http.StatusBadRequest)
			return
		}
	
		response, err := h.service.SignalBuild(r.Context(), buildID, &signalReq)
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

type statusWriterImpl struct {
	w      http.ResponseWriter
	enc    *json.Encoder
	flusher http.Flusher
}

func NewStatusWriter(w http.ResponseWriter) StatusWriter {
	sw := &statusWriterImpl{
		w:      w,
		enc:    json.NewEncoder(w),
		flusher: w.(http.Flusher),
	}
	return sw
}

func (sw *statusWriterImpl) Started(rsp *BuildStarted) error {
	if err := sw.enc.Encode(rsp); err != nil {
		return err
	}
	sw.flusher.Flush()
	return nil
}

func (sw *statusWriterImpl) Updated(update *StatusUpdate) error {
	if err := sw.enc.Encode(update); err != nil {
		return err
	}
	sw.flusher.Flush()
	return nil
}

func (sw *statusWriterImpl) Error(err error) error {
    response := StatusUpdate{
        BuildFailed: &BuildFailed{Error: err.Error()},
    }
    encodeErr := sw.enc.Encode(response);
	if encodeErr != nil {
        return encodeErr
    }
    sw.flusher.Flush()
    return nil
}
