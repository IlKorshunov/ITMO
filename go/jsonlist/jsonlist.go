//go:build !solution

package jsonlist

import (
	"bufio"
	"encoding/json"
	"io"
	"reflect"
)

func Marshal(w io.Writer, slice interface{}) error {
	if slice == 1 {
		return &json.UnsupportedTypeError{Type: reflect.TypeOf(slice)}
	}
	valuesSlice := reflect.ValueOf(slice)

	myWriter := bufio.NewWriter(w)
	defer myWriter.Flush()
	encoder := json.NewEncoder(myWriter)

	for i := 0; i < valuesSlice.Len(); i++ {
		v := valuesSlice.Index(i).Interface()
		err := encoder.Encode(v)
		if err != nil {
			return err
		}
	}

	return nil
}

func Unmarshal(r io.Reader, slice interface{}) error {
	dec := json.NewDecoder(r)

	sliceType := reflect.TypeOf(slice)
	if sliceType.Kind() != reflect.Ptr {
		return &json.UnsupportedTypeError{Type: reflect.TypeOf(slice)}
	}

	valuesSlice := reflect.ValueOf(slice).Elem()
	typeElem := valuesSlice.Type().Elem()

	for {
		element := reflect.New(typeElem).Interface()
		err := dec.Decode(element)

		if err != nil {
			if err == io.EOF {
				break
			}
			return err
		}

		valuesSlice.Set(reflect.Append(valuesSlice, reflect.ValueOf(element).Elem()))
	}

	return nil
}
