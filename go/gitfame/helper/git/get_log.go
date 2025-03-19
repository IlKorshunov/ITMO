package git

import (
	"os/exec"
	"strings"
)

type AuthorInfo struct {
	Hash   string
	Author string
}

// GetLastAuthor возвращает хэш и автора последнего коммита для заданного файла
func GetAuthors(mgi *MyGitInfo, commitHash string, name string) (*AuthorInfo, error) {

	cmd := exec.Command("git", "-C", mgi.Directory, "log", commitHash, "-1", "--pretty=format:%H %an", "--", name)
	output, err := cmd.Output()
	if err != nil {
		return nil, err
	}

	line := strings.TrimSpace(string(output))
	parts := strings.SplitN(line, " ", 2)
	if len(parts) < 2 {
		return nil, nil
	}

	return &AuthorInfo{
		Hash:   parts[0],
		Author: parts[1],
	}, nil
}
