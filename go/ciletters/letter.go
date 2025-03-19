//go:build !solution

package ciletters

import (
	"bytes"
	_ "embed"
	"strings"
	"text/template"
)

//go:embed helper/file.txt
var content string

func cut(input string) []string {
	lines := strings.Split(input, "\n")
	var out []string
	if len(lines)-10 > 0 {
		out = lines[len(lines)-10:]
	} else {
		out = lines
	}
	return out
}

func hash(input string) string {
	return input[:8]
}

func MakeLetter(n *Notification) (string, error) {
	tmpl := template.New("myTemplate")
	funcMap := template.FuncMap{
		"hash": hash,
		"cut":  cut,
	}
	tmpl, err := tmpl.Funcs(funcMap).Parse(content)
	if err != nil {
		return "", err
	}

	var buf bytes.Buffer
	err = tmpl.Execute(&buf, n)
	if err != nil {
		return "", err
	}

	return buf.String(), nil
}
