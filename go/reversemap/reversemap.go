//go:build !solution

package reversemap

import "reflect"

func ReverseMap(forward interface{}) interface{} {
	if reflect.ValueOf(forward).Kind() != reflect.Map {
		panic("Not a map")
	}

	out := reflect.MakeMap(reflect.MapOf(reflect.ValueOf(forward).Type().Elem(), reflect.ValueOf(forward).Type().Key()))

	for _, key := range reflect.ValueOf(forward).MapKeys() {
		curValue := reflect.ValueOf(forward).MapIndex(key)
		out.SetMapIndex(curValue, key)
	}

	return out.Interface()
}
