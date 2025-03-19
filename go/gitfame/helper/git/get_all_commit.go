package git

import (
	"os/exec"
	"strings"
)

func GetALLCommits(mgi *MyGitInfo) ([]string, error) {
	cmd := exec.Command("git", "log", "--pretty=format:%H", mgi.Revision) // ,
	cmd.Dir = mgi.Directory
	commit, err := cmd.Output()
	if err != nil {
		return nil, err
	}
	commits := strings.Split(strings.TrimSpace(string(commit)), "\n")
	return commits, nil
}
