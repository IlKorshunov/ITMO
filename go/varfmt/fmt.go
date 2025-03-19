package varfmt

import (
	"fmt"
	"strconv"
	"strings"
)

func Sprintf(format string, args ...interface{}) string {
	var out strings.Builder
	out.Grow(3 * len(format))
	counter := 0
	i := 0

	for i < len(format) {
		if format[i] == '{' {
			j := i + 1
			for j < len(format) && format[j] != '}' {
				j++
			}
			indexStr := format[i+1 : j]
			if indexStr == "" {
				fmt.Fprintf(&out, "%v", args[counter])
				counter++
			} else {
				argIndex, _ := strconv.Atoi(indexStr)
				fmt.Fprintf(&out, "%v", args[argIndex])
				counter++
			}
			i = j + 1
		} else {
			out.WriteByte(format[i])
			i++
		}
	}

	return out.String()
}
