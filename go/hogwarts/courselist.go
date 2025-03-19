//go:build !solution

package hogwarts

// func reverseSlice(s []string) {
// 	for i, j := 0, len(s)-1; i < j; i, j = i+1, j-1 {
// 		s[i], s[j] = s[j], s[i]
// 	}
// }

func dfs(course string, prereqs map[string][]string, used map[string]bool, finished map[string]bool, out *[]string, hasCycle *bool) {
	used[course] = true
	for _, prer := range prereqs[course] {
		if !used[prer] {
			dfs(prer, prereqs, used, finished, out, hasCycle)
		} else if !finished[prer] {
			*hasCycle = true
			return
		}
	}
	finished[course] = true
	*out = append(*out, course)
}

func topologicalSort(prereqs map[string][]string, used map[string]bool, finished map[string]bool, out *[]string, hasCycle *bool) {
	for course := range prereqs {
		used[course] = false
		finished[course] = false
	}
	for course := range prereqs {
		if *hasCycle {
			break
		}
		if !used[course] {
			dfs(course, prereqs, used, finished, out, hasCycle)
		}
	}
	//reverseSlice(*out)
}

func GetCourseList(prereqs map[string][]string) []string {
	used := make(map[string]bool)
	finished := make(map[string]bool)
	var out []string
	hasCycle := false

	topologicalSort(prereqs, used, finished, &out, &hasCycle)

	if hasCycle {
		panic("prereqs have cycle")
	}
	return out
}
