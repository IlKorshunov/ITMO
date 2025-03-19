//go:build !solution

package api

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"net/http"

	"go.uber.org/zap"

	"gitlab.com/slon/shad-go/distbuild/pkg/build"
)

type BuildClient struct {
	logger   *zap.Logger
	endpoint string
}

func NewBuildClient(l *zap.Logger, endpoint string) *BuildClient {
	return &BuildClient{
		logger:   l,
		endpoint: endpoint,
	}
}

func (c *BuildClient) StartBuild(ctx context.Context, request *BuildRequest) (*BuildStarted, StatusReader, error) {
    c.logger.Info("Start building")
    curRequest := c.endpoint + "/build"
    data, err := json.Marshal(request)
    if err != nil {
        c.logger.Error("Failed to encode request", zap.Error(err))
        return nil, nil, err
    }
    
    req, err := http.NewRequestWithContext(ctx, "POST", curRequest, bytes.NewBuffer(data))
    if err != nil {
        c.logger.Error("Failed to create request", zap.Error(err))
        return nil, nil, err
    }
    req.Header.Add("Content-Type", "application/json")
    
    client := &http.Client{}
    resp, err := client.Do(req)
    if err != nil {
        c.logger.Error("Failed to send request", zap.Error(err))
        return nil, nil, err
    }
    
	reader := NewStatusReader(resp)

    if resp.StatusCode != http.StatusOK {
		c.logger.Error("Non-OK HTTP status", zap.Int("status", resp.StatusCode))
        var errResp map[string]string
        err = json.NewDecoder(resp.Body).Decode(&errResp)
        if err != nil {
            c.logger.Error("Failed to decode error response", zap.Error(err))
            return nil, nil, err
        }
        return nil, nil, fmt.Errorf(errResp["error"])
    }

    var buildResponse BuildStarted
    err = json.NewDecoder(resp.Body).Decode(&buildResponse)
    if err != nil {
		c.logger.Error("Failed to decode successful response", zap.Error(err))
        return nil, nil, err
    }


    return &buildResponse, reader, nil
}


func (c *BuildClient) SignalBuild(ctx context.Context, buildID build.ID, signal *SignalRequest) (*SignalResponse, error) {
	//c.logger.Info("Sending signal to build")
	data, err := json.Marshal(signal)
	if err != nil {
		c.logger.Error("Failed to encode signal request", zap.Error(err))
		return nil, err
	}

	build_Id := fmt.Sprintf("build_id=%s", buildID)
	url := c.endpoint + "/signal?" + build_Id

	req, _ := http.NewRequestWithContext(ctx, "POST", url, bytes.NewBuffer(data))
	req.Header.Add("Content-Type", "application/json")
	
	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		c.logger.Error("Failed to send signal request", zap.Error(err))
		return nil, err
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		var errResp map[string]string
		if err := json.NewDecoder(resp.Body).Decode(&errResp); err != nil {
			c.logger.Error("Failed to decode error response", zap.Error(err))
			return nil, err
		}
		return nil, fmt.Errorf(errResp["error"])
	}

	var signalResponse SignalResponse
	if err := json.NewDecoder(resp.Body).Decode(&signalResponse); err != nil {
		return nil, err
	}

	return &signalResponse, nil
}

type statusReaderImpl struct {
	decoder *json.Decoder
	resp *http.Response
}

func NewStatusReader(resp *http.Response) StatusReader {
	return &statusReaderImpl{
		decoder: json.NewDecoder(resp.Body),
		resp: resp,
	}
}

func (r *statusReaderImpl) Close() error {
	return r.resp.Body.Close()
}

func (r *statusReaderImpl) Next() (*StatusUpdate, error) {
	var update StatusUpdate
	err := r.decoder.Decode(&update)
	if err != nil {
		return nil, err
	}
	return &update, nil
}