//go:build !solution

package filecache

import (
	"context"
	"fmt"
	"io"
	"net/http"
	"os"

	"gitlab.com/slon/shad-go/distbuild/pkg/build"
	"go.uber.org/zap"
)

type Client struct {
	logger *zap.Logger
	endpoint string
}

func NewClient(l *zap.Logger, endpoint string) *Client {
	return &Client{
		logger: l,
		endpoint: endpoint,
	}
}

func (c *Client) Upload(ctx context.Context, id build.ID, localPath string) error {
	Id := fmt.Sprintf("id=%s", id)
	url := c.endpoint + "/file?" + Id

	file, err := os.Open(localPath)
	if err != nil {
		return err
	}

	req, err := http.NewRequestWithContext(ctx, "PUT", url, file)
	if err != nil {
		return err
	}

	client := &http.Client{}
	_, _ = client.Do(req)

	return nil
}

func (c *Client) Download(ctx context.Context, localCache *Cache, id build.ID) error {
    idParam := fmt.Sprintf("id=%s", id)
    url := c.endpoint + "/file?" + idParam

    req, err := http.NewRequestWithContext(ctx, "GET", url, nil)
    if err != nil {
        return err
    }

    client := &http.Client{}
    resp, err := client.Do(req)
    if err != nil {
        return err
    }
    defer resp.Body.Close()

    if resp.StatusCode != http.StatusOK {
        return fmt.Errorf("err status: %d", resp.StatusCode)
    }

    writer, abort, err := localCache.Write(id)
    if err != nil {
        return err
    }

    if _, err := io.Copy(writer, resp.Body); err != nil {
        abort()
        return err
    }

    if err := writer.Close(); err != nil {
        return err
    }

    return nil
}

