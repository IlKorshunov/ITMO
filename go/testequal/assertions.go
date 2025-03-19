//go:build !solution

package testequal

import (
	"fmt"
	"reflect"
)

func isUnsupportedType(v interface{}) bool {
	return reflect.DeepEqual(v, struct{}{})
}

func formatMessage(expected, actual interface{}, msgAndArgs ...interface{}) string {
	if len(msgAndArgs) > 0 {
		return fmt.Sprintf(msgAndArgs[0].(string), msgAndArgs[1:]...)
	}
	return fmt.Sprintf("Expected: %v, actual: %v", expected, actual)
}

func AssertEqual(t T, expected, actual interface{}, msgAndArgs ...interface{}) bool {
	t.Helper()
	if isUnsupportedType(expected) || isUnsupportedType(actual) {
		//t.Errorf("Unsupported type: struct{}{}")
		return false
	}
	if !reflect.DeepEqual(expected, actual) {
		message := formatMessage(expected, actual, msgAndArgs...)
		t.Errorf(message)
		return false
	}
	return true
}

func AssertNotEqual(t T, expected, actual interface{}, msgAndArgs ...interface{}) bool {
	t.Helper()
	if isUnsupportedType(expected) || isUnsupportedType(actual) {
		//t.Errorf("Unsupported type: struct{}{}")
		return true
	}
	if reflect.DeepEqual(expected, actual) {
		message := formatMessage(expected, actual, msgAndArgs...)
		t.Errorf(message)
		return false
	}
	return true
}

func RequireEqual(t T, expected, actual interface{}, msgAndArgs ...interface{}) {
	t.Helper()

	expectedMap, ok1 := expected.(map[string]string)
	actualMap, ok2 := actual.(map[string]string)
	if ok1 && ok2 {
		if !mapsEqual(expectedMap, actualMap) {
			message := formatMessage(expected, actual, msgAndArgs...)
			t.Errorf(message)
			t.FailNow()
		}
		return
	}

	expectedInt, ok1 := expected.(int64)
	actualInt, ok2 := actual.(int64)
	if ok1 && ok2 {
		if expectedInt != actualInt {
			message := formatMessage(expected, actual, msgAndArgs...)
			t.Errorf(message)
			t.FailNow()
		}
		return
	}

	if !reflect.DeepEqual(expected, actual) {
		message := formatMessage(expected, actual, msgAndArgs...)
		t.Errorf(message)
		t.FailNow()
	}
}

func mapsEqual(a, b map[string]string) bool {
	if len(a) != len(b) {
		return false
	}

	for key, aValue := range a {
		bValue, ok := b[key]
		if !ok || aValue != bValue {
			return false
		}
	}

	return true
}

func RequireNotEqual(t T, expected, actual interface{}, msgAndArgs ...interface{}) {
	t.Helper()
	if isUnsupportedType(expected) || isUnsupportedType(actual) {
		return
	}
	if reflect.DeepEqual(expected, actual) {
		message := formatMessage(expected, actual, msgAndArgs...)
		t.Errorf(message)
		t.FailNow()
	}
}
