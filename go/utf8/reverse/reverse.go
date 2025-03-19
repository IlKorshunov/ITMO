package reverse

import (
	"strings"
	"unicode/utf8"
)

func Reverse(input string) string {
	var result strings.Builder
	result.Grow(len(input))

	for i := len(input); i > 0; {
		runeValue, size := utf8.DecodeLastRuneInString(input[:i])
		if runeValue == utf8.RuneError && (size == 1 || size > utf8.UTFMax) {
			result.WriteRune('\uFFFD')
		} else {
			result.WriteRune(runeValue)
		}
		i -= size
	}
	return result.String()
}
