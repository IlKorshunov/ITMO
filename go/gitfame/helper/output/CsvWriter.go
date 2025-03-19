package output

import (
	"encoding/csv"
	"fmt"
	"gitlab.com/slon/shad-go/gitfame/helper/struct"
	"io"
)

type CSVWriter struct{}

func (c CSVWriter) CSVWrite(authors []structures.Author, w io.Writer) error {
	myWriter := csv.NewWriter(w)
	header := []string{"Name", "Lines", "Commits", "Files"}
	if err := myWriter.Write(header); err != nil {
		return err
	}
	for _, author := range authors {
		if author.Files == 0 {
			continue
		}
		row := []string{
			author.Name,
			fmt.Sprintf("%d", author.Lines),
			fmt.Sprintf("%d", author.Commits),
			fmt.Sprintf("%d", author.Files),
		}
		if err := myWriter.Write(row); err != nil {
			return err
		}
	}
	myWriter.Flush()
	return nil
}
