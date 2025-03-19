package main

import (
	"encoding/json"
	"fmt"
	"github.com/spf13/pflag"

	"gitlab.com/slon/shad-go/gitfame/helper/git"
	"gitlab.com/slon/shad-go/gitfame/helper/output"
	"gitlab.com/slon/shad-go/gitfame/helper/sorting"
	structures "gitlab.com/slon/shad-go/gitfame/helper/struct"

	"os"
	"strings"
)

type MyGitInfo struct {
	structures.GitInfo
}

type Lang struct {
	Name       string   `json:"name"`
	Type       string   `json:"type"`
	Extensions []string `json:"extensions"`
}

type allLangs []Lang

func main() {
	var extensions []string
	var languages []string
	var exclude []string
	var restrict []string

	repoPath := pflag.String("repository", ".", "путь до Git репозитория")
	revision := pflag.String("revision", "HEAD", "способ сослаться на Git объект")

	pflag.StringSliceVar(&extensions, "extensions", []string{}, "список расширений")
	pflag.StringSliceVar(&languages, "languages", []string{}, "список языков")
	pflag.StringSliceVar(&exclude, "exclude", []string{}, "набор Glob паттернов, исключающих файлы из расчёта")
	pflag.StringSliceVar(&restrict, "restrict-to", []string{}, "набор Glob паттернов, исключающий все файлы, не удовлетворяющие ни одному из паттернов набора")

	usecommiter := pflag.Bool("use-committer", false, "учитывать автора или коммитера")
	ordering := pflag.String("order-by", "lines", "порядок сортировки")
	outputFormat := pflag.String("format", "tabular", "формат вывода результатов")
	pflag.Parse()

	getSlice := getSliceLang(languages)

	gitInfo := structures.GitInfo{Directory: *repoPath, Revision: *revision,
		Extensions: extensions, Languages: getSlice, UseCommitter: *usecommiter, Restrict: restrict}

	mgi := git.MyGitInfo{
		GitInfo: gitInfo,
	}

	authors, err := git.GetAllInfo(&mgi, exclude)

	if err != nil {
		fmt.Println("Ошибка :", err)
		os.Exit(1)
	}
	sorting.LocalSort(authors, *ordering)

	switch *outputFormat {
	case "tabular":
		tw := output.TabWriter{}
		err := tw.TabWrite(authors, os.Stdout)
		if err != nil {
			fmt.Println("Ошибка при выводе таблицы:", err)
		}
	case "csv":
		cw := output.CSVWriter{}
		err := cw.CSVWrite(authors, os.Stdout)
		if err != nil {
			fmt.Println("Ошибка при выводе CSV:", err)
		}
	case "json":
		cw := output.JSONWriter{}
		err := cw.JSONWrite(authors, os.Stdout)
		if err != nil {
			fmt.Println("Ошибка при выводе json:", err)
		}
	case "json-lines":
		cw := output.JSONWriterLines{}
		err := cw.JSONWriterLines(authors, os.Stdout)
		if err != nil {
			fmt.Println("Ошибка при выводе json-lines:", err)
		}
	default:
		os.Exit(1)
	}
}

func getSliceLang(langs []string) []string {
	data, err := os.ReadFile("../../configs/language_extensions.json")
	if err != nil {
		return nil
	}
	mySlice := []string{}
	var languages allLangs

	err = json.Unmarshal(data, &languages)
	if err != nil {
		return nil
	}

	for _, curLang := range languages {
		flag := false
		for _, lang := range langs {
			if strings.ToLower(curLang.Name) == lang {
				flag = true
			}
		}

		if flag {
			mySlice = append(mySlice, curLang.Extensions...)
		}
	}

	return mySlice
}
