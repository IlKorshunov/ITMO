package artifact

import (
	"context"
	"fmt"
	"net/http"

	"gitlab.com/slon/shad-go/distbuild/pkg/build"
	"gitlab.com/slon/shad-go/distbuild/pkg/tarstream"
)

// Download artifact from remote cache into local cache.
// c - туда, куда я хочу сохранить данные из artifactID
func Download(ctx context.Context, endpoint string, c *Cache, artifactID build.ID) error {
	select {
	case <-ctx.Done():
		return ctx.Err()
	default:
		id := fmt.Sprintf("id=%s", artifactID)
		url := endpoint + "/artifact?" + id

		request, err := http.NewRequestWithContext(ctx, "GET", url, nil)
		if err != nil {	
			return err
		}
		client := &http.Client{}
		resp, err := client.Do(request)
		
		if err != nil {
			return err
		}

		path, commit, _, err := c.Create(artifactID)
		if err != nil {
			return err
		}

		err = tarstream.Receive(path, resp.Body)

		if err != nil {
			return err
		}
		return commit()
	}
}

