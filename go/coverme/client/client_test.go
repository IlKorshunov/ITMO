package client

import (
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"reflect"
	"strings"
	"testing"

	"github.com/stretchr/testify/require"
	"gitlab.com/slon/shad-go/coverme/models"
)

func TestAdd(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path != "/todo/create" {
			http.Error(w, "incorrect url", http.StatusNotFound)
			return
		}
		if r.Method != "POST" {
			http.Error(w, "isn't post", http.StatusMethodNotAllowed)
			return
		}
		var req models.AddRequest
		if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
			http.Error(w, "Bad Request", http.StatusBadRequest)
			return
		}
		if req.Title == "" {
			http.Error(w, "err in title", http.StatusBadRequest)
			return
		}
		resp := models.Todo{
			ID:       1,
			Title:    req.Title,
			Content:  req.Content,
			Finished: false,
		}
		w.WriteHeader(http.StatusCreated)
		err := json.NewEncoder(w).Encode(resp)
		if err != nil {
			return
		}
	}))
	defer server.Close()

	client := New(server.URL)

	testCases := []struct {
		name     string
		request  *models.AddRequest
		wantTodo *models.Todo
		wantErr  bool
	}{
		{
			name:     "simple good",
			request:  &models.AddRequest{Title: "Test", Content: "Do something"},
			wantTodo: &models.Todo{ID: 1, Title: "Test", Content: "Do something", Finished: false},
			wantErr:  false,
		},
		{
			name:    "missing",
			request: &models.AddRequest{Content: "Do something"},
			wantErr: true,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			todo, err := client.Add(tc.request)
			if tc.wantErr {
				require.Error(t, err)
			} else {
				require.NoError(t, err)
				require.True(t, reflect.DeepEqual(todo, tc.wantTodo))
			}
		})
	}
}

func TestError(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path == "/todo" && r.Method == "GET" {
			w.WriteHeader(http.StatusServiceUnavailable)
			_, err := w.Write([]byte("Service Unavailable"))
			if err != nil {
				return
			}
			return
		}
		http.NotFound(w, r)
	}))
	defer server.Close()

	client := New(server.URL)

	_, err := client.List()
	if err == nil {
		t.Errorf("error in status")
	}
}

func TestGet(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch r.URL.Path {
		case "/todo/1":
			todo := models.Todo{ID: 1, Title: "Test", Content: "Content", Finished: false}
			err := json.NewEncoder(w).Encode(todo)
			if err != nil {
				return
			}
		default:
			http.NotFound(w, r)
		}
	}))
	defer server.Close()

	client := New(server.URL)

	t.Run("successful retrieval", func(t *testing.T) {
		expected := &models.Todo{ID: 1, Title: "Test", Content: "Content", Finished: false}
		todo, err := client.Get(1)
		require.NoError(t, err)
		require.True(t, reflect.DeepEqual(todo, expected))
	})

	t.Run("not found", func(t *testing.T) {
		_, err := client.Get(999)
		require.Error(t, err)
	})
}

func TestList(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path != "/todo" {
			http.Error(w, "Not Found", http.StatusNotFound)
			return
		}
		if r.Method != "GET" {
			http.Error(w, "Method Not Allowed", http.StatusMethodNotAllowed)
			return
		}
		todos := []*models.Todo{
			{ID: 1, Title: "First Task", Content: "Details", Finished: false},
			{ID: 2, Title: "Second Task", Content: "Details", Finished: true},
		}
		err := json.NewEncoder(w).Encode(todos)
		if err != nil {
			return
		}
	}))
	defer server.Close()

	client := New(server.URL)

	todos, err := client.List()
	require.NoError(t, err)
	require.Len(t, todos, 2)
	require.Equal(t, "First Task", todos[0].Title)
	require.Equal(t, true, todos[1].Finished)
}

func TestFinish(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if !strings.HasPrefix(r.URL.Path, "/todo/") {
			http.Error(w, "errr", http.StatusMethodNotAllowed)
			return
		}
		switch r.URL.Path {
		case "/todo/1/finish":
			w.WriteHeader(http.StatusOK)
		default:
			http.NotFound(w, r)
		}
	}))
	defer server.Close()

	client := New(server.URL)

	t.Run("successful finish", func(t *testing.T) {
		err := client.Finish(1)
		require.NoError(t, err)
	})

	t.Run("not found", func(t *testing.T) {
		err := client.Finish(999)
		require.Error(t, err)
	})
}

/*
func TestAdd(t *testing.T) {
	httpmock.Activate()
	defer httpmock.DeactivateAndReset()

	client := New("https://www.university.edu")

	testCases := []struct {
		name     string
		status   int
		todo     *models.Todo
		response string
		willErr  bool
	}{
		{
			name:     "simple add",
			status:   http.StatusCreated,
			todo:     &models.Todo{ID: 0, Title: "Title", Content: "Content", Finished: false},
			response: `{"ID":0,"Title":"Title","Content":"Content","Finished":false}`,
			willErr:  false,
		},
		{
			name:     "error add",
			status:   http.StatusInternalServerError,
			todo:     &models.Todo{ID: 0, Title: "Title", Content: "Content", Finished: false},
			response: `{"error":"Internal Server Error"}`,
			willErr:  true,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			httpmock.RegisterResponder("POST", "https://www.university.edu/todo/create",
				httpmock.NewStringResponder(tc.status, tc.response))

			expectedTodo, err := client.Add(&models.AddRequest{
				Title:   tc.todo.Title,
				Content: tc.todo.Content,
			})

			if tc.willErr {
				require.Error(t, err)
				require.Nil(t, expectedTodo)
			} else {
				require.NoError(t, err)
				require.NotNil(t, expectedTodo)
				require.True(t, reflect.DeepEqual(expectedTodo, tc.todo))
			}
		})
	}
}

func TestGet(t *testing.T) {
	httpmock.Activate()
	defer httpmock.DeactivateAndReset()
	client := New("https://www.university.edu")

	testcases := []struct {
		name     string
		id       models.ID
		response string
		todo     *models.Todo
		willErr  bool
		status   int
	}{
		{
			name:     "simple get",
			id:       0,
			response: `{"ID":0,"Title":"Title","Content":"Content","Finished":false}`,
			todo:     &models.Todo{ID: 0, Title: "Title", Content: "Content", Finished: false},
			willErr:  false,
			status:   http.StatusOK,
		},
		{
			name:     "test with server error",
			id:       1000,
			response: `{"error":"Internal Server Error"}`,
			todo:     nil,
			willErr:  true,
			status:   http.StatusInternalServerError,
		},
	}

	for _, tc := range testcases {
		url := fmt.Sprintf("https://www.university.edu/todo/%d", tc.id)
		httpmock.RegisterResponder("GET", url,
			httpmock.NewStringResponder(tc.status, tc.response))

		getTodo, err := client.Get(tc.id)

		if tc.willErr {
			if err == nil {
				t.Errorf("%s: Expected error but got none", tc.name)
			} else {
				if tc.status == http.StatusInternalServerError && !strings.Contains(err.Error(), "500") {
					t.Errorf("%s: Expected HTTP 500: %v", tc.name, err)
				}
			}
		} else if err != nil {
			t.Errorf("%s: Client.Get() unexpected error: %v", tc.name, err)
			continue
		}

		if !reflect.DeepEqual(getTodo, tc.todo) {
			t.Errorf("%s: Client.Get() = %v, want %v", tc.name, getTodo, tc.todo)
		}
	}
}

func TestList(t *testing.T) {
	httpmock.Activate()
	defer httpmock.DeactivateAndReset()
	client := New("https://www.university.edu")

	testCases := []struct {
		name       string
		mockStatus int
		mockBody   string
		wantTodos  []*models.Todo
		wantErr    bool
	}{
		{
			name:       "successful list",
			mockStatus: http.StatusOK,
			mockBody:   `[{"ID":1,"Title":"First","Content":"First content","Finished":false},{"ID":2,"Title":"Second","Content":"Second content","Finished":true}]`,
			wantTodos: []*models.Todo{
				{ID: 1, Title: "First", Content: "First content", Finished: false},
				{ID: 2, Title: "Second", Content: "Second content", Finished: true},
			},
			wantErr: false,
		},
		{
			name:       "server error",
			mockStatus: http.StatusInternalServerError,
			mockBody:   ``,
			wantTodos:  nil,
			wantErr:    true,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			httpmock.RegisterResponder("GET", "https://www.university.edu/todo",
				httpmock.NewStringResponder(tc.mockStatus, tc.mockBody))

			gotTodos, err := client.List()

			if (err != nil) != tc.wantErr {
				t.Errorf("Client.List() error = %v, wantErr %v", err, tc.wantErr)
				return
			}
			if !tc.wantErr && !reflect.DeepEqual(gotTodos, tc.wantTodos) {
				t.Errorf("Client.List() = %v, want %v", gotTodos, tc.wantTodos)
			}
		})
	}
}

func TestFinish(t *testing.T) {
	httpmock.Activate()
	defer httpmock.DeactivateAndReset()
	client := New("https://www.university.edu")

	testcases := []struct {
		name     string
		id       models.ID
		response string
		willErr  bool
		status   int
	}{
		{
			name:     "simple finish",
			id:       0,
			response: `{"ID":0,"Title":"Title","Content":"Content","Finished":true}`,
			willErr:  false,
			status:   200,
		},
		{
			name:     "error finish",
			id:       500,
			response: "unexpected status code 500",
			willErr:  true,
			status:   500,
		},
	}

	for _, tc := range testcases {
		url := fmt.Sprintf("https://www.university.edu/todo/%d/finish", tc.id)
		httpmock.RegisterResponder("POST", url,
			httpmock.NewStringResponder(tc.status, tc.response))

		err := client.Finish(tc.id)

		if tc.willErr {
			if err == nil {
				t.Errorf("%s: Client.Finish() expected error, got none", tc.name)
			} else {
				if !strings.Contains(err.Error(), fmt.Sprintf("unexpected status code %d", tc.status)) {
					t.Errorf("%s: Client.Finish() unexpected error message: %v", tc.name, err)
				}
			}
		} else if err != nil {
			t.Errorf("%s: Client.Finish() unexpected error: %v", tc.name, err)
		}
	}
}
///////
func TestNetworkErrors(t *testing.T) {
	httpmock.Activate()
	defer httpmock.DeactivateAndReset()

	client := New("https://www.university.edu")
	testCases := []struct {
		name    string
		method  string
		url     string
		handler func(req *http.Request) (*http.Response, error)
		action  func() error
	}{
		{
			name:   "Add network error",
			method: "POST",
			url:    "https://www.university.edu/todo/create",
			handler: func(req *http.Request) (*http.Response, error) {
				return nil, fmt.Errorf("network error")
			},
			action: func() error {
				_, err := client.Add(&models.AddRequest{Title: "Test", Content: "Content"})
				return err
			},
		},
		{
			name:   "Get network error",
			method: "GET",
			url:    fmt.Sprintf("https://www.university.edu/todo/%d", 1),
			handler: func(req *http.Request) (*http.Response, error) {
				return nil, fmt.Errorf("network error")
			},
			action: func() error {
				_, err := client.Get(1)
				return err
			},
		},
		{
			name:   "Finish network error",
			method: "POST",
			url:    fmt.Sprintf("https://www.university.edu/todo/%d/finish", 1),
			handler: func(req *http.Request) (*http.Response, error) {
				return nil, fmt.Errorf("network error")
			},
			action: func() error {
				err := client.Finish(1)
				return err
			},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			httpmock.RegisterResponder(tc.method, tc.url, tc.handler)
			err := tc.action()
			if err == nil {
				t.Errorf("%s: Expect errror", tc.name)
			} else if !strings.Contains(err.Error(), "network error") {
				t.Errorf("%s: expect nerwork error: %v", tc.name, err)
			}
		})
	}
}

*/
