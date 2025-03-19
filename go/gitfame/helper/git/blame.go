package git

import (
	"os/exec"
	"regexp"
	"strconv"
	"strings"
)

type AllInfo struct {
	commits []string // вместо сета лучше использовать массив
}

var commitLineRegex = regexp.MustCompile(`^[a-f0-9]{40}\s+\d+\s+\d+\s+\d+$`)

func parseBlameOutput(input string) (string, int, bool) {
	if commitLineRegex.MatchString(input) {
		hash, count := handleCommitLine(input)
		return hash, count, true
	}
	return "-1", -1, false
}

func handleCommitLine(line string) (string, int) {
	parts := strings.Split(line, " ")
	commitHash := parts[0]
	lines, _ := strconv.Atoi(parts[3])
	return commitHash, lines
}

// GetBlameInfo возвращает информацию о том, сколько строк принадлежит каждому автору в файле
func GetBlameInfo(mgi *MyGitInfo, Name string) (map[string]AllInfo, map[string]int, error) {

	cmd := exec.Command("git", "blame", Name, "--porcelain", mgi.Revision) // --incremental
	cmd.Dir = mgi.Directory
	output, err := cmd.Output()
	if err != nil {
		return nil, nil, err
	}

	lines := strings.Split(string(output), "\n")

	commits := make(map[string]int)
	localLines := 0
	var willAuth bool
	var lastHash string
	authorInfo := make(map[string]AllInfo)

	for _, line := range lines {
		if localLines == 0 {
			curHash, count, _ := parseBlameOutput(line) // пытался сделать через регулярки и incremental
			// но не получилось
			localLines = count
			commits[curHash] += localLines
			willAuth = true
			lastHash = curHash
		} else if willAuth && line[0] != '\t' {
			var namePrefix string
			if !mgi.UseCommitter {
				namePrefix = "author "
			} else {
				namePrefix = "committer "
			}
			if strings.HasPrefix(line, namePrefix) {
				name := strings.TrimPrefix(line, namePrefix)
				authorInfo = updateAuthorInfo(authorInfo, name, lastHash)
				willAuth = false
			}
		} else if line[0] == '\t' {
			localLines--
		}
	}

	if err != nil {
		return nil, nil, err
	}

	return authorInfo, commits, nil
}

func updateAuthorInfo(authorInfo map[string]AllInfo, name string, lastHash string) map[string]AllInfo {
	cur := authorInfo[name]
	cur.commits = append(cur.commits, lastHash)
	authorInfo[name] = cur
	return authorInfo
}

func GetAuthorFile(mgi *MyGitInfo, Name string) (string, error) {
	cmd := exec.Command("git", "log", "--format=%an", "--diff-filter=A", "--", Name)
	cmd.Dir = mgi.Directory
	output, err := cmd.Output()
	if err != nil {
		return "", err
	}
	outputStr := strings.TrimSpace(string(output))
	authors := strings.Split(outputStr, "\n")
	if len(authors) > 0 {
		return authors[len(authors)-1], nil
	}
	return "", nil
}
