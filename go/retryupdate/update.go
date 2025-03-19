package retryupdate

import (
	"errors"
	"github.com/gofrs/uuid"
	"gitlab.com/slon/shad-go/retryupdate/kvapi"
)

func MySet(c kvapi.Client, key string, newValue string, oldVersion uuid.UUID) (uuid.UUID, error) {
	newVersion := uuid.Must(uuid.NewV4())
	_, err := c.Set(&kvapi.SetRequest{
		Key:        key,
		Value:      newValue,
		OldVersion: oldVersion,
		NewVersion: newVersion,
	})
	return newVersion, err
}

func UpdateValue(c kvapi.Client, key string, updateFn func(oldValue *string) (newValue string, err error)) error {
	var lastVersion uuid.UUID
	var newVersion uuid.UUID
	for {
		response, err := c.Get(&kvapi.GetRequest{Key: key})
		var currentValue *string
		var authErr *kvapi.AuthError
		if err != nil {
			if errors.Is(err, kvapi.ErrKeyNotFound) {
				currentValue = nil
				lastVersion = uuid.Nil
			} else if errors.As(err, &authErr) {
				return err
			} else {
				continue
			}
		} else {
			currentValue = &response.Value
			lastVersion = response.Version
		}

		newValue, err := updateFn(currentValue)
		if err != nil {
			return err
		}

		newVersion, err = MySet(c, key, newValue, lastVersion)
		if err != nil {
			var apiErr *kvapi.APIError
			var conflictErr *kvapi.ConflictError
			var authErr *kvapi.AuthError
			if errors.Is(err, kvapi.ErrKeyNotFound) {
				currentValue = nil
				lastVersion = uuid.Nil
				newValue, _ = updateFn(currentValue)
			}
			if errors.As(err, &authErr) {
				return err
			}
			if errors.As(err, &conflictErr) {
				response, _ = c.Get(&kvapi.GetRequest{Key: key})
				newValue, err = updateFn(&response.Value)
				if err != nil {
					return err
				}
			}
			if errors.As(err, &apiErr) {
				_, err = MySet(c, key, newValue, lastVersion)
				if err == nil {
					return err
				}
				unwrappedErr := errors.Unwrap(err)
				var conflictErr *kvapi.ConflictError
				if errors.As(unwrappedErr, &conflictErr) {
					if conflictErr.ExpectedVersion == newVersion {
						return nil
					}
				}
				continue
			}
		} else {
			return nil
		}
	}
}
