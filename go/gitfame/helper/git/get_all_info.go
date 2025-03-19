package git

import (
	"fmt"
	"path"
	"path/filepath"

	"gitlab.com/slon/shad-go/gitfame/helper/struct"
)

type MyGitInfo struct {
	structures.GitInfo
}

func GetAllInfo(mgi *MyGitInfo, exclude []string) ([]structures.Author, error) {
	var authors []structures.Author
	authorStats := make(map[string]*structures.Author)

	commits, err := GetALLCommits(mgi)
	if err != nil {
		return nil, err
	}
	mgi.Commit = commits

	files, err := TreeTrav(mgi)
	if err != nil {
		return nil, err
	}

	if len(files) == 0 {
		return nil, nil
	}

	for _, file := range files {
		myMap, newMap, err := GetBlameInfo(mgi, file)
		if err != nil {
			fmt.Println("Ошибка info:", err)
			return nil, err
		}
		if len(myMap) == 0 {
			Author, _ := GetAuthors(mgi, mgi.Revision, file)
			author, ok := authorStats[Author.Author]
			if !ok {
				author = &structures.Author{
					Name:       Author.Author,
					CommitsSet: make(map[string]struct{}),
					Commits:    0,
					Lines:      0,
					Files:      0,
				}
				authorStats[Author.Author] = author
			}
			author.Files++
			author.CommitsSet[Author.Hash] = struct{}{}
			continue
		}
		flag, err := mgi.CheckFileFlags(file, exclude)
		if err != nil {
			return nil, err
		}
		if !flag {
			continue
		}
		for key, info := range myMap {
			author, ok := authorStats[key]
			if !ok {
				author = &structures.Author{
					Name:       key,
					CommitsSet: make(map[string]struct{}),
					Commits:    0,
					Lines:      0,
					Files:      0,
				}
				authorStats[key] = author
			}
			for _, curHash := range info.commits {
				author.Lines += newMap[curHash]
				author.CommitsSet[curHash] = struct{}{}
			}
			author.Files++
		}
	}

	for _, author := range authorStats {
		author.Commits = len(author.CommitsSet)
		if author.Commits > 0 && author.Files > 0 {
			authors = append(authors, *author)
		}
	}

	return authors, nil
}

func (mgi *MyGitInfo) CheckFileFlags(Name string, exclude []string) (bool, error) {
	fileExtension := filepath.Ext(Name)

	for _, ex := range exclude {
		match, err := path.Match(ex, Name)
		if err != nil {
			return false, err
		}
		if match {
			return false, nil
		}
	}

	flagRest := len(mgi.Restrict) == 0
	flagExt := len(mgi.Extensions) == 0
	flagLang := len(mgi.Languages) == 0

	for _, restr := range mgi.Restrict {
		match, err := path.Match(restr, Name)
		if err != nil {
			return false, err
		}
		if match {
			flagRest = true
			break
		}
	}

	for _, ext := range mgi.Extensions {
		if ext == fileExtension {
			flagExt = true
			break
		}
	}

	for _, lang := range mgi.Languages {
		if lang == fileExtension {
			flagLang = true
			break
		}
	}

	return flagRest && flagExt && flagLang, nil
}
