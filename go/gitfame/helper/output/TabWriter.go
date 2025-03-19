package output

import (
	"fmt"
	"gitlab.com/slon/shad-go/gitfame/helper/struct"
	"io"
	"text/tabwriter"
)

type TabWriter struct{}

func (t TabWriter) TabWrite(authors []structures.Author, w io.Writer) error {
	myWriter := tabwriter.NewWriter(w, 1, 1, 1, ' ', 0)
	_, err := fmt.Fprint(myWriter, "Name\tLines\tCommits\tFiles\n")
	if err != nil {
		return err
	}
	for _, auth := range authors {
		_, err := fmt.Fprintf(myWriter, "%s\t%d\t%d\t%d\n", auth.Name, auth.Lines, auth.Commits, auth.Files)
		if err != nil {
			return err
		}
	}
	if err := myWriter.Flush(); err != nil {
		return err
	}
	return nil
}
