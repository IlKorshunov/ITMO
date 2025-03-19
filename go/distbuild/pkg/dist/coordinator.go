//go:build !solution

package dist

import (
	"context"
	"encoding/json"
	"net/http"
	"time"

	"go.uber.org/zap"

	"gitlab.com/slon/shad-go/distbuild/pkg/api"
	"gitlab.com/slon/shad-go/distbuild/pkg/build"
	"gitlab.com/slon/shad-go/distbuild/pkg/filecache"
	"gitlab.com/slon/shad-go/distbuild/pkg/scheduler"
)

type Coordinator struct {
	log *zap.Logger
	fileCache *filecache.Cache
	scheduler *scheduler.Scheduler
}

var defaultConfig = scheduler.Config{
	CacheTimeout: time.Millisecond * 10,
	DepsTimeout:  time.Millisecond * 100,
}

func NewCoordinator(
	log *zap.Logger,
	fileCache *filecache.Cache,
) *Coordinator {
	return &Coordinator{
		log: log,
		fileCache: fileCache,
		scheduler: scheduler.NewScheduler(log, defaultConfig),
	}
}

func (c *Coordinator) Stop() {}

func (c *Coordinator) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	switch r.Method{
	case http.MethodPost:
		switch r.URL.Path {
		case "/heartbeat":
			c.handleBeat(w, r)
		case "/build":
			c.handleBuild(w, r)
		case "/signal":
			c.handleSignal(w, r)
		}

	case http.MethodGet:

	}
}

func (c *Coordinator) handleBeat(w http.ResponseWriter, r *http.Request) {
	var heartbeatRequest api.HeartbeatRequest
	if err := json.NewDecoder(r.Body).Decode(&heartbeatRequest); err != nil {
		http.Error(w, "Invalid request body", http.StatusBadRequest)
		return
	}

	response := api.HeartbeatResponse{
		JobsToRun: map[build.ID]api.JobSpec{}, 
	}

	for i := 0; i < heartbeatRequest.FreeSlots; i++{
		job := c.scheduler.PickJob(context.Background(), heartbeatRequest.WorkerID)
        if job == nil {
            break 
        }
		response.JobsToRun[job.Job.Job.ID] = *job.Job
	}

	err := json.NewEncoder(w).Encode(response)
	if err != nil {
		http.Error(w, "Error during encoding by coordinator", http.StatusBadRequest)
		return
	}

}

func (c *Coordinator) handleBuild(w http.ResponseWriter, r *http.Request) {
	var GetGraph api.BuildRequest
	if err := json.NewDecoder(r.Body).Decode(&GetGraph); err != nil {
		http.Error(w, "Invalid request body", http.StatusBadRequest)
		return
	}

	c.log.Info("Starting build process for new graph")

	for _, curJob := range GetGraph.Graph.Jobs{
		curJobSpec := api.JobSpec{
            SourceFiles: GetGraph.Graph.SourceFiles,
            Artifacts:   map[build.ID]api.WorkerID{}, // что сюда следует положить?
            Job:         curJob,
		}
		c.scheduler.ScheduleJob(&curJobSpec) // по идее это мне вернет PendingJob и его надо как-то обработать
	}

	w.WriteHeader(http.StatusOK)
}


func (c *Coordinator) handleSignal(w http.ResponseWriter, r *http.Request) {
	c.log.Info("Signal received from worker")
	//buildID := r.URL.Query().Get("build_id")
	var getSignal api.SignalRequest 
	decoder := json.NewDecoder(r.Body)
	err := decoder.Decode(&getSignal)
	if err != nil {
		c.log.Info("error during decoding signal in coordinator")
		return
	}
	if getSignal.UploadDone != nil {
		// надо как-то это обработать
	}
	var outSignal api.SignalResponse
	encoder := json.NewEncoder(w)
	err = encoder.Encode(outSignal)
	if err != nil {
		c.log.Info("error during encoding signal in coordinator")
		return
	}
	w.WriteHeader(http.StatusOK)
}