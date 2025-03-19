package speller

import (
	"strings"
)

var (
	ones  = []string{"", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"}
	teens = []string{"ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"}
	tens  = []string{"", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"}
)

func Spell(n int64) string {
	if n == 0 {
		return "zero"
	}
	var ans []string
	if n < 0 {
		ans = append(ans, "minus")
		n = -n
	}

	parts := []struct {
		value int64
		name  string
	}{
		{1e12, " trillion"},
		{1e9, " billion"},
		{1e6, " million"},
		{1e3, " thousand"},
		{1, ""},
	}

	for _, part := range parts {
		if n >= part.value {
			ans = append(ans, hundredToWords(n/part.value)+part.name)
			n %= part.value
		}
	}

	return strings.TrimSpace(strings.Join(ans, " "))
}

func hundredToWords(n int64) string {
	var out strings.Builder
	if n >= 100 {
		out.WriteString(ones[n/100] + " hundred")
		n %= 100
		if n > 0 {
			out.WriteString(" ")
		}
	}

	if n >= 20 {
		out.WriteString(tens[n/10])
		n %= 10
		if n > 0 {
			out.WriteString("-" + ones[n])
		}
	} else if n >= 10 {
		out.WriteString(teens[n-10])
	} else if n > 0 {
		out.WriteString(ones[n])
	}

	return out.String()
}
