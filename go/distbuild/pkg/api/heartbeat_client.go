//go:build !solution

package api

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"net/http"

	"go.uber.org/zap"
)

type HeartbeatClient struct {
	logger  *zap.Logger
	endpoint string
}

func NewHeartbeatClient(l *zap.Logger, endpoint string) *HeartbeatClient {
	return &HeartbeatClient{
		logger: l,
		endpoint: endpoint,
	}
}

func (c *HeartbeatClient) Heartbeat(ctx context.Context, req *HeartbeatRequest) (*HeartbeatResponse, error) {
    select {
    case <- ctx.Done():
        c.logger.Info("Heartbeat cancelled because ctx has been Done")
        return nil, ctx.Err()
    default:
        c.logger.Info("Starting heartbeat")
        url := c.endpoint + "/heartbeat"
        data, err := json.Marshal(req)
        if err != nil {
            c.logger.Error("Failed to encode request", zap.Error(err))
            return nil, err
        }
    
        request, err := http.NewRequestWithContext(ctx, "POST", url, bytes.NewBuffer(data))
        if err != nil {
            c.logger.Error("Failed to create request", zap.Error(err))
            return nil, err
        }
        request.Header.Add("Content-Type", "application/json")
    
        client := &http.Client{}
        resp, err := client.Do(request)
        if err != nil {
            c.logger.Error("Failed to send request", zap.Error(err))
            return nil, err
        }
        defer resp.Body.Close()
        var out HeartbeatResponse
        decoder := json.NewDecoder(resp.Body)
    
        if resp.StatusCode != http.StatusOK {
            var errResp map[string]string
            if err := decoder.Decode(&errResp); err != nil {
                c.logger.Error("Failed to decode error response", zap.Error(err))
                return nil, err
            }
            return nil, fmt.Errorf(errResp["error"])
        }
    
        if err := decoder.Decode(&out); err != nil {
            c.logger.Error("Failed to decode response", zap.Error(err))
            return nil, err
        }
    
        return &out, nil
    }
   
}

