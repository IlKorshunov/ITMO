package spacecollapse

import (
	"strings"
	"unicode"
	"unicode/utf8"
)

func CollapseSpaces(input string) string {
	var result strings.Builder
	flag := false
	result.Grow(len(input))
	for i := 0; i < len(input); {
		runeValue, width := utf8.DecodeRuneInString(input[i:])
		if runeValue == utf8.RuneError || width > utf8.UTFMax || width == 0 {
			result.WriteRune('\uFFFD')
			flag = false
		} else if unicode.IsSpace(runeValue) {
			if !flag {
				result.WriteRune(' ')
			}
			flag = true
		} else {
			flag = false
			result.WriteString(string(runeValue))
		}
		i += width
	}

	return result.String()
}
