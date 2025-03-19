package utils

import (
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
)

////

func TestRespondJSON(t *testing.T) {
	type TestData struct {
		Message string `json:"message"`
	}

	data := TestData{Message: "Test Message"}
	expected := `{"message":"Test Message"}`

	w := httptest.NewRecorder()

	err := RespondJSON(w, http.StatusOK, data)

	if w.Code != http.StatusOK {
		t.Errorf("errr %d", w.Code)
	}

	if err != nil {
		t.Errorf("U get such error: %v", err)
	}

	body := strings.TrimSpace(w.Body.String())
	if body != expected {
		t.Errorf("Unexpected body %q", body)
	}
}

func TestServerError(t *testing.T) {
	w := httptest.NewRecorder()

	ServerError(w)
}

func TestBadRequest(t *testing.T) {
	message := "Bad Request"
	w := httptest.NewRecorder()

	BadRequest(w, message)
}
