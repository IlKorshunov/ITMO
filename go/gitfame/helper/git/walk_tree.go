package git

import (
	"os/exec"
	"strings"
)

func TreeTrav(mgi *MyGitInfo) ([]string, error) {
	cmd := exec.Command("git", "ls-tree", "-r", "--name-only", mgi.Revision)
	cmd.Dir = mgi.Directory
	fileNames, err := cmd.Output()
	if len(fileNames) == 0 {
		return nil, nil
	}
	if err != nil {
		return nil, err
	}
	output := strings.Split(strings.TrimSpace(string(fileNames)), "\n")
	return output, nil
}
