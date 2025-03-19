package worker

import (
	"context"
	"fmt"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"time"

	"go.uber.org/zap"

	"gitlab.com/slon/shad-go/distbuild/pkg/api"
	"gitlab.com/slon/shad-go/distbuild/pkg/artifact"
	"gitlab.com/slon/shad-go/distbuild/pkg/build"
	"gitlab.com/slon/shad-go/distbuild/pkg/filecache"
)

type Worker struct {
	workerID             api.WorkerID
	coordinatorEndpoint  string
	log                  *zap.Logger
	fileCache            *filecache.Cache
	artifacts            *artifact.Cache
}

func New(
	workerID api.WorkerID,
	coordinatorEndpoint string,
	log *zap.Logger,
	fileCache *filecache.Cache,
	artifacts *artifact.Cache,
) *Worker {
	return &Worker{
		workerID:            workerID,
		coordinatorEndpoint: coordinatorEndpoint,
		log:                 log,
		fileCache:           fileCache,
		artifacts:           artifacts,
	}
}

func (w *Worker) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	w.log.Info("HTTP server started in worker")
	// Implement the actual HTTP handler logic here.
}

func (w *Worker) Run(ctx context.Context) error {
	heartbeatClient := api.NewHeartbeatClient(w.log, w.coordinatorEndpoint)
	ticker := time.NewTicker(1 * time.Second)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			return ctx.Err()
		case <-ticker.C:
			request := api.HeartbeatRequest{
				WorkerID:      w.workerID,
				RunningJobs:   []build.ID{},
				FinishedJob:   []api.JobResult{},
				AddedArtifacts: []build.ID{},
				FreeSlots:     10, 
			}
			response, err := heartbeatClient.Heartbeat(ctx, &request)
			if err != nil {
				w.log.Error("Failed during heartbeat", zap.Error(err))
				continue
			}
			for jobID, jobSpec := range response.JobsToRun {
				go w.runJob(ctx, jobID, jobSpec, w.log, w.coordinatorEndpoint, request, heartbeatClient)  
			}
			
		}
	}
}

func (w *Worker) runJob(ctx context.Context, jobID build.ID, jobSpec api.JobSpec, logger *zap.Logger, endpoint string, request api.HeartbeatRequest, myClient *api.HeartbeatClient) {
	client := filecache.NewClient(logger, endpoint)
	// Установка контекста с таймаутом для выполнения каждой команды
	jobCtx, cancel := context.WithTimeout(ctx, time.Second * 5)
	defer cancel()

	for fileID := range jobSpec.SourceFiles {
		if _, unlock, err := w.fileCache.Get(fileID); err != nil {
			if err = client.Download(ctx, w.fileCache, fileID); err != nil {
				w.log.Error("Failed to download file", zap.String("fileID", fileID.String()), zap.Error(err))
				return
			}
			defer unlock()
		}
	}

	jobContext := build.JobContext{
		SourceDir: jobID.Path(), // Должен возвращать путь к директории с исходными файлами
		OutputDir: endpoint, // Путь к выходной директории
		Deps:      make(map[build.ID]string),
	}

	for _, cmd := range jobSpec.Job.Cmds {
		output, err := executeJobCommand(jobCtx, cmd, jobContext)
		if err != nil {
			logger.Error("Failed to execute job command", zap.Error(err))
			continue
		}
		logger.Info("Command executed successfully", zap.String("output", output))
	}
	// Запись в log об успешном выполнении всех команд
	logger.Info("All job commands executed successfully")
}


func executeJobCommand(ctx context.Context, cmd build.Cmd, jobCtx build.JobContext) (string, error) {
    renderedCmd, err := cmd.Render(jobCtx)
    if err != nil {
        return "", err
    }

    var command *exec.Cmd
    if len(renderedCmd.Exec) > 0 {
        command = exec.CommandContext(ctx, renderedCmd.Exec[0], renderedCmd.Exec[1:]...)
        command.Dir = renderedCmd.WorkingDirectory
        command.Env = append(os.Environ(), renderedCmd.Environ...)
        output, err := command.CombinedOutput()
        if err != nil {
            return "", fmt.Errorf("command execution failed: %v, output: %s", err, string(output))
        }
        return string(output), nil
    } else if renderedCmd.CatTemplate != "" {
        outputPath := filepath.Join(renderedCmd.WorkingDirectory, renderedCmd.CatOutput)
        if err := os.WriteFile(outputPath, []byte(renderedCmd.CatTemplate), 0644); err != nil {
            return "", err
        }
        return "File created", nil
    }

    return "", fmt.Errorf("no executable command or cat template provided")
}
