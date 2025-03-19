//go:build !solution

package auth

import (
	"context"
	"errors"
	"net/http"
	"strings"
)

type contextKey string

const userKey contextKey = "user"

type User struct {
	Name  string
	Email string
}

func ContextUser(ctx context.Context) (*User, bool) {
	user, ok := ctx.Value(userKey).(*User)
	return user, ok
}

var ErrInvalidToken = errors.New("invalid token")

type TokenChecker interface {
	CheckToken(ctx context.Context, token string) (*User, error)
}

func CheckAuth(checker TokenChecker) func(next http.Handler) http.Handler {

	handler := func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			getString := r.Header.Get("authorization")
			ans := strings.Split(getString, " ")
			if len(ans) != 2 || ans[0] != "Bearer" {
				http.Error(w, "Unauthorized", http.StatusUnauthorized)
				return
			}

			user, err := checker.CheckToken(r.Context(), ans[1])

			if err != nil {
				if errors.Is(err, ErrInvalidToken) {
					http.Error(w, "Invalid token", http.StatusUnauthorized)
				} else {
					http.Error(w, "Server error", http.StatusInternalServerError)
				}
				return
			}

			ctx := context.WithValue(r.Context(), userKey, user)
			r = r.WithContext(ctx)

			next.ServeHTTP(w, r)
		})
	}

	return func(next http.Handler) http.Handler {
		return handler(next)
	}
}
