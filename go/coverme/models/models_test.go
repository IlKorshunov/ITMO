package models

import (
	"reflect"
	"testing"

	"github.com/stretchr/testify/require"
)

// //////
func TestAdd(t *testing.T) {
	myStorage := NewInMemoryStorage()

	testcases := []struct {
		name     string
		title    string
		content  string
		response string
		todo     *Todo
	}{
		{
			name:     "simple test",
			title:    "Title",
			content:  "Content",
			todo:     &Todo{ID: 0, Title: "Title", Content: "Content", Finished: false},
			response: `{"ID":0,"Title":"Title","Content":"Content","Finished":false}`,
		},
	}

	for _, tc := range testcases {
		expectedTodo, _ := myStorage.AddTodo(tc.title, tc.content)
		require.True(t, reflect.DeepEqual(expectedTodo, tc.todo))
	}
}

func TestGet(t *testing.T) {
	myStorage := NewInMemoryStorage()

	testcases := []struct {
		name string
		id   int
		todo *Todo
	}{
		{
			name: "simple test",
			id:   0,
			todo: &Todo{ID: 0, Title: "Title", Content: "Content", Finished: false},
		},
	}

	for _, tc := range testcases {
		_, err := myStorage.AddTodo(tc.todo.Title, tc.todo.Content)
		if err != nil {
			continue
		}
		expectedTodo, _ := myStorage.GetTodo(tc.todo.ID)
		require.True(t, reflect.DeepEqual(expectedTodo, tc.todo))
	}
}

func TestGetAll(t *testing.T) {
	myStorage := NewInMemoryStorage()

	testcases := []struct {
		name       string
		todosToAdd []*Todo
	}{
		{
			name: "Simple test with multiple todos",
			todosToAdd: []*Todo{
				{ID: 0, Title: "Title1", Content: "Content1", Finished: false},
				{ID: 1, Title: "Title2", Content: "Content2", Finished: false},
			},
		},
	}

	for _, tc := range testcases {
		t.Run(tc.name, func(t *testing.T) {
			for _, todo := range tc.todosToAdd {
				_, err := myStorage.AddTodo(todo.Title, todo.Content)
				require.NoError(t, err)
			}

			gotTodos, err := myStorage.GetAll()

			require.NoError(t, err)
			require.Equal(t, len(tc.todosToAdd), len(gotTodos))

			for _, want := range tc.todosToAdd {
				found := false
				for _, got := range gotTodos {
					if reflect.DeepEqual(got, want) {
						found = true
						break
					}
				}
				require.True(t, found, "got unexpected todo:", want)
			}
		})
	}
}

func TestFinish(t *testing.T) {
	myStorage := NewInMemoryStorage()

	testcases := []struct {
		name    string
		id      int
		todo    *Todo
		willErr bool
	}{
		{
			name:    "simple test",
			id:      0,
			todo:    &Todo{ID: 0, Title: "Title", Content: "Content", Finished: false},
			willErr: false,
		},
	}

	for _, tc := range testcases {
		_, err := myStorage.AddTodo(tc.todo.Title, tc.todo.Content)
		if err != nil {
			continue
		}
		err = myStorage.FinishTodo(tc.todo.ID)
		if err != nil {
			continue
		}
		err = myStorage.FinishTodo(tc.todo.ID)
		if !tc.willErr {
			require.NoError(t, err)
		}
	}
}

func TestExist(t *testing.T) {
	myStorage := NewInMemoryStorage()
	_, err := myStorage.GetTodo(999)
	require.Error(t, err, "should error when todo does not exist")
}
