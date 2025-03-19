package output

import (
	"encoding/json"
	"gitlab.com/slon/shad-go/gitfame/helper/struct"
	"io"
)

type JSONWriterLines struct{}

func (jw JSONWriterLines) JSONWriterLines(authors []structures.Author, w io.Writer) error {
	for _, author := range authors {
		data, err := json.Marshal(author)
		if err != nil {
			return err
		}
		if _, err := w.Write(data); err != nil {
			return err
		}
		if _, err := w.Write([]byte("\n")); err != nil {
			return err
		}
	}
	return nil
}
