package structures

type Author struct {
	Commits    int                 `json:"commits"`
	Files      int                 `json:"files"`
	Lines      int                 `json:"lines"`
	Name       string              `json:"name"`
	CommitsSet map[string]struct{} `json:"-"`
}

type GitInfo struct {
	Authors      []Author
	Directory    string
	Commit       []string
	Revision     string
	Extensions   []string
	Restrict     []string
	Languages    []string
	UseCommitter bool
	Ordering     string
}
