package sorting

import (
	"os"

	"strings"

	"gitlab.com/slon/shad-go/gitfame/helper/struct"
)

func Len(people []structures.Author) int {
	return len(people)
}

func Less(person []structures.Author, kind string, i, j int) bool {
	I := strings.ToLower(strings.Fields(strings.TrimSpace(person[i].Name))[0])
	J := strings.ToLower(strings.Fields(strings.TrimSpace(person[j].Name))[0])

	var key1, key2 []int
	switch kind {
	case "commits":
		key1 = []int{person[i].Commits, person[i].Lines, person[i].Files}
		key2 = []int{person[j].Commits, person[j].Lines, person[j].Files}
	case "files":
		key1 = []int{person[i].Files, person[i].Lines, person[i].Commits}
		key2 = []int{person[j].Files, person[j].Lines, person[j].Commits}
	case "lines":
		key1 = []int{person[i].Lines, person[i].Commits, person[i].Files}
		key2 = []int{person[j].Lines, person[j].Commits, person[j].Files}
	default:
		os.Exit(1)
	}

	for level := 0; level < len(key1); level++ {
		if key1[level] == key2[level] {
			continue
		}
		return key1[level] < key2[level]
	}

	return I > J
}

func Swap(people []structures.Author, i, j int) {
	people[i], people[j] = people[j], people[i]
}

func LocalSort(people []structures.Author, kind string) {
	sortByKind(people, kind)
}

func sortByKind(people []structures.Author, kind string) {
	for i := 0; i < Len(people)-1; i++ {
		for j := i + 1; j < Len(people); j++ {
			if Less(people, kind, i, j) {
				Swap(people, i, j)
			}
		}
	}
}
