//go:build !solution

package structtags

import (
	"fmt"
	"net/http"
	"reflect"
	"strconv"
	//"strings"
	"sync"
)

func myLower(str string) string {
	myStr := make([]byte, len(str))
	for i, ch := range str {
		if 'A' <= ch && ch <= 'Z' {
			myStr[i] = byte(ch) + 'a' - 'A'
		} else {
			myStr[i] = byte(ch)
		}
	}
	return string(myStr)
}

var Cache sync.Map

func Unpack(req *http.Request, ptr interface{}) error {

	if err := req.ParseForm(); err != nil {
		return err
	}

	v := reflect.ValueOf(ptr).Elem()
	t := v.Type()

	loadFields, ok := Cache.Load(t)
	if !ok {
		fields := make(map[string]reflect.Value, v.NumField())
		for i := 0; i < v.NumField(); i++ {
			fieldInfo := t.Field(i)
			tag := fieldInfo.Tag
			name := tag.Get("http")
			if name == "" {
				name = myLower(fieldInfo.Name) //strings.ToLower(fieldInfo.Name)
			}
			fields[name] = v.Field(i)
		}
		loadFields = fields
		Cache.Store(t, fields)
	}

	for name, values := range req.Form {
		f, ok := loadFields.(map[string]reflect.Value)[name]
		if !ok {
			continue
		}

		sliceType := f.Type()

		if f.Kind() == reflect.Slice {
			slice := reflect.MakeSlice(sliceType, len(values), len(values))
			elemType := sliceType.Elem()
			for i, value := range values {
				elem := reflect.New(elemType).Elem()
				if err := populate(elem, value); err != nil {
					return fmt.Errorf("%s: %v", name, err)
				}
				slice.Index(i).Set(elem)
			}
			f.Set(slice)
		} else {
			if err := populate(f, values[0]); err != nil {
				return fmt.Errorf("%s: %v", name, err)
			}
		}

	}
	return nil
}

func populate(v reflect.Value, value string) error {
	switch v.Kind() {
	case reflect.String:
		v.SetString(value)

	case reflect.Int:
		i, err := strconv.ParseInt(value, 10, 64)
		if err != nil {
			return err
		}
		v.SetInt(i)

	case reflect.Bool:
		b, err := strconv.ParseBool(value)
		if err != nil {
			return err
		}
		v.SetBool(b)

	default:
		return fmt.Errorf("unsupported kind %s", v.Type())
	}
	return nil
}
