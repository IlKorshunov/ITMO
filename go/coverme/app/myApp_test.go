package app

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"net/http"
	"net/http/httptest"
	"reflect"
	"strconv"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"gitlab.com/slon/shad-go/coverme/models"
)

type ErrorDB struct{}

func (s *ErrorDB) GetAll() ([]*models.Todo, error) {
	return nil, errors.New("err")
}

func (s *ErrorDB) AddTodo(title, content string) (*models.Todo, error) {
	return nil, errors.New("err")
}

func (s *ErrorDB) GetTodo(id models.ID) (*models.Todo, error) {
	return nil, errors.New("err")
}

func (s *ErrorDB) FinishTodo(id models.ID) error {
	return errors.New("err")
}

func TestMyAdd(t *testing.T) {
	var db = models.NewInMemoryStorage()

	testcases := []struct {
		name     string
		status   int
		title    string
		content  string
		db       models.Storage
		willErr  bool
		response string
	}{
		{
			name:    "correct",
			db:      db,
			status:  http.StatusCreated,
			title:   "title",
			content: "content",
			willErr: false,
		},
		{
			name:    "storage",
			db:      &ErrorDB{},
			status:  http.StatusInternalServerError,
			willErr: true,
		},
		{
			name:     "missing",
			db:       db,
			response: `{"content":"content"}`,
			status:   http.StatusBadRequest,
			willErr:  true,
		},
	}

	for _, tc := range testcases {
		t.Run(tc.name, func(t *testing.T) {
			localApp := New(tc.db)
			localApp.initRoutes()
			w := httptest.NewRecorder()
			var jsonData []byte
			var err error

			if tc.response == "" {
				data := map[string]string{
					"title":   tc.title,
					"content": tc.content,
				}
				jsonData, err = json.Marshal(data)
				require.NoError(t, err)
			} else {
				jsonData = []byte(tc.response)
			}

			body := bytes.NewBuffer(jsonData)
			req := httptest.NewRequest("POST", "/todo/create", body)
			localApp.router.ServeHTTP(w, req)

			if !tc.willErr {
				var get models.Todo
				err = json.NewDecoder(w.Body).Decode(&get)
				require.NoError(t, err, "error decoding response")

				expect, err := tc.db.AddTodo(tc.title, tc.content)
				require.NoError(t, err, "error adding todo")

				valExpected := reflect.ValueOf(expect).Elem()
				valActual := reflect.ValueOf(&get).Elem()

				for i := 1; i < valExpected.NumField(); i++ {
					if !reflect.DeepEqual(valExpected.Field(i).Interface(), valActual.Field(i).Interface()) {
						t.Fatalf("Some fields don't equals")
					}
				}
			}
		})
	}
}

func TestMyGet(t *testing.T) {
	localDB := models.NewInMemoryStorage()
	todo, _ := localDB.AddTodo("Test Title", "Test Content")

	testcases := []struct {
		name    string
		status  int
		id      string
		db      models.Storage
		willErr bool
	}{
		{
			name:    "successful retrieval",
			db:      localDB,
			status:  http.StatusOK,
			id:      strconv.Itoa(int(todo.ID)),
			willErr: false,
		},
		{
			name:    "todo not found",
			db:      localDB,
			status:  http.StatusNotFound,
			id:      "9999",
			willErr: true,
		},
		{
			name:    "invalid ID format",
			db:      localDB,
			status:  http.StatusBadRequest,
			id:      "abc",
			willErr: true,
		},
		{
			name:    "error in storage",
			db:      &ErrorDB{},
			status:  http.StatusInternalServerError,
			id:      "0",
			willErr: true,
		},
	}

	for _, tc := range testcases {
		t.Run(tc.name, func(t *testing.T) {
			app := New(tc.db)
			app.initRoutes()
			w := httptest.NewRecorder()
			req := httptest.NewRequest("GET", fmt.Sprintf("/todo/%s", tc.id), nil)
			app.router.ServeHTTP(w, req)

			if !tc.willErr {
				require.Equal(t, tc.status, w.Code, fmt.Sprintf("err %d, got %d", tc.status, w.Code))
			}

			if !tc.willErr {
				var get models.Todo
				err := json.NewDecoder(w.Body).Decode(&get)
				require.NoError(t, err, "decoding should succeed")
				expect, _ := tc.db.GetTodo(todo.ID)
				require.Equal(t, expect, &get, "err")
			}
		})
	}
}

func TestMyList(t *testing.T) {
	localDB := models.NewInMemoryStorage()
	for i := 0; i < 10; i++ {
		_, err := localDB.AddTodo(fmt.Sprintf("Task %d", i), fmt.Sprintf("Content %d", i))
		if err != nil {
			continue
		}
	}

	errorDB := &ErrorDB{}

	testcases := []struct {
		name    string
		status  int
		db      models.Storage
		willErr bool
	}{
		{
			name:    "successful listing",
			status:  http.StatusOK,
			db:      localDB,
			willErr: false,
		},
		{
			name:    "database error",
			status:  http.StatusInternalServerError,
			db:      errorDB,
			willErr: true,
		},
	}

	for _, tc := range testcases {
		t.Run(tc.name, func(t *testing.T) {
			app := New(tc.db)
			app.initRoutes()
			w := httptest.NewRecorder()
			req := httptest.NewRequest("GET", "/todo", nil)
			app.router.ServeHTTP(w, req)

			if tc.willErr {
				require.NotEqual(t, http.StatusOK, w.Code, "errr")
			} else {
				require.Equal(t, tc.status, w.Code, "errr")
				var todos []*models.Todo
				err := json.NewDecoder(w.Body).Decode(&todos)
				require.NoError(t, err, "errr")
			}
		})
	}
}

func TestMyFinish(t *testing.T) {
	localDB := models.NewInMemoryStorage()
	todo, _ := localDB.AddTodo("Test Todo", "Test Content")

	testcases := []struct {
		name    string
		status  int
		id      string
		db      models.Storage
		willErr bool
	}{
		{
			name:    "successful finish",
			status:  http.StatusOK,
			id:      strconv.Itoa(int(todo.ID)),
			db:      localDB,
			willErr: false,
		},
		{
			name:    "invalid ID format",
			status:  http.StatusBadRequest,
			id:      "invalid",
			db:      localDB,
			willErr: true,
		},
		{
			name:    "error in storage",
			status:  http.StatusInternalServerError,
			id:      strconv.Itoa(int(todo.ID)),
			db:      &ErrorDB{},
			willErr: true,
		},
		{
			name:   "id not int",
			db:     localDB,
			id:     "92233720368547758088",
			status: http.StatusBadRequest,
		},
	}

	for _, tc := range testcases {
		t.Run(tc.name, func(t *testing.T) {
			app := New(tc.db)
			app.initRoutes()
			w := httptest.NewRecorder()
			req := httptest.NewRequest("POST", fmt.Sprintf("/todo/%s/finish", tc.id), nil)
			app.router.ServeHTTP(w, req)

			if tc.willErr {
				require.NotEqual(t, http.StatusOK, w.Code, "erre")
			} else {
				require.Equal(t, tc.status, w.Code, fmt.Sprintf("expected status code %d, got %d", tc.status, w.Code))
			}
		})
	}
}

////////

func TestApp_Status(t *testing.T) {
	app := New(nil)
	app.initRoutes()

	req, _ := http.NewRequest("GET", "/", nil)
	w := httptest.NewRecorder()

	app.router.ServeHTTP(w, req)

	assert.Equal(t, http.StatusOK, w.Code, "err")
}

func TestJson(t *testing.T) {
	localApp := New(&ErrorDB{})
	localApp.initRoutes()
	w := httptest.NewRecorder()
	body := bytes.NewBufferString(`{"title": "incomplete json"`)
	req := httptest.NewRequest("POST", "/todo/create", body)
	localApp.router.ServeHTTP(w, req)

	require.Equal(t, http.StatusBadRequest, w.Code, "err")
}

func TestStart(t *testing.T) {
	app := New(nil)
	go app.Start(8080)
	time.Sleep(time.Second)

	resp, err := http.Get("http://localhost:8080/")
	require.NoError(t, err)
	defer resp.Body.Close()
	assert.Equal(t, http.StatusOK, resp.StatusCode)
}

func TestRun(t *testing.T) {
	app := New(nil)
	go func() {
		app.initRoutes()
		app.run(":8091")
	}()
	time.Sleep(time.Second)

	resp, err := http.Get("http://localhost:8091/")
	require.NoError(t, err)
	defer resp.Body.Close()
	assert.Equal(t, http.StatusOK, resp.StatusCode)
}
