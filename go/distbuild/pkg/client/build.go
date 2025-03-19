package client

import (
	"context"
	"io"

	"go.uber.org/zap"

	"gitlab.com/slon/shad-go/distbuild/pkg/api"
	"gitlab.com/slon/shad-go/distbuild/pkg/artifact"
	"gitlab.com/slon/shad-go/distbuild/pkg/build"
)

type Client struct {
	logger *zap.Logger
	apiEndpoint string
	sourceDir string
}

func NewClient(l *zap.Logger, apiEndpoint string, sourceDir string) *Client {
	return &Client{
		logger: l,
		apiEndpoint: apiEndpoint,
		sourceDir: sourceDir,
	}
}

type BuildListener interface {
	OnJobStdout(jobID build.ID, stdout []byte) error
	OnJobStderr(jobID build.ID, stderr []byte) error
	OnJobFinished(jobID build.ID) error
	OnJobFailed(jobID build.ID, code int, error string) error
}

func (c *Client) Build(ctx context.Context, graph build.Graph, lsn BuildListener) error {
	clientApi := api.NewBuildClient(c.logger, c.apiEndpoint)
	request := api.BuildRequest{
		Graph: graph,
	}
	buildStarted, reader, err := clientApi.StartBuild(ctx, &request)
	if err != nil {
		c.logger.Error("Failed to start build", zap.Error(err))
		return err
	}
	defer reader.Close()

	cache, err := artifact.NewCache(c.sourceDir)
	if err != nil {
		c.logger.Error("Failed to create artifact cache", zap.Error(err))
		return err
	}

	// здесь надо как-то уснуть
	for _, id := range buildStarted.MissingFiles {
		if err = artifact.Download(ctx, c.apiEndpoint, cache, id); err != nil {
			c.logger.Error("Failed to download missing file", zap.String("fileID", id.String()), zap.Error(err))
			return err
		}
	}

	go func() {
		for {
			curCode, err := reader.Next()
			if err == io.EOF {
				break
			}
			if err != nil {
				c.logger.Error("Failed to read status update", zap.Error(err))
				return
			}

			if curCode.JobFinished != nil {
				if err := lsn.OnJobFinished(curCode.JobFinished.ID); err != nil {
					c.logger.Error("Failed to notify job finish", zap.Error(err))
				}
			}

			if curCode.BuildFinished != nil {
				break
			}
		}
	}()

	return nil
}

