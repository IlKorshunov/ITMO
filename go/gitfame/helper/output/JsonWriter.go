package output

import (
	"encoding/json"
	"gitlab.com/slon/shad-go/gitfame/helper/struct"
	"io"
)

type JSONWriter struct{}

func (jw JSONWriter) JSONWrite(authors []structures.Author, w io.Writer) error {
	encoder := json.NewEncoder(w)
	return encoder.Encode(authors)
}
