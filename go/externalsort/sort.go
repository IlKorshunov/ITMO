package externalsort

import (
	"bytes"
	"container/heap"
	"io"
	"os"
	"sort"
)

type fileLineReader struct {
	reader io.Reader
}

func (flr *fileLineReader) ReadLine() (string, error) {
	var line bytes.Buffer
	buf := make([]byte, 1)

	for {
		n, err := flr.reader.Read(buf)
		if n > 0 {
			if buf[0] == '\n' {
				break
			}
			line.WriteByte(buf[0])
		}
		if err != nil && buf[0] != '\r' {
			if err == io.EOF {
				return line.String(), io.EOF
			}
			return "", err
		}
	}

	return line.String(), nil
}

type fileLineWriter struct {
	writer io.Writer
}

func (flw *fileLineWriter) Write(l string) error {
	data := []byte(l + "\n")
	n, err := flw.writer.Write(data)
	if err != nil {
		return err
	}
	if n < len(data) {
		return io.ErrShortWrite
	}
	return nil
}

func NewReader(r io.Reader) LineReader {
	return &fileLineReader{reader: r}
}

func NewWriter(w io.Writer) LineWriter {
	return &fileLineWriter{writer: w}
}

type Item struct {
	reader LineReader
	value  string
}

type PriorityQueue []*Item

func (pq PriorityQueue) Len() int {
	return len(pq)
}

func (pq PriorityQueue) Less(i, j int) bool {
	return pq[i].value < pq[j].value
}

func (pq PriorityQueue) Swap(i, j int) {
	pq[i], pq[j] = pq[j], pq[i]
}

func (pq *PriorityQueue) Push(x interface{}) {
	current := x.(*Item)
	*pq = append(*pq, current)
	heap.Fix(pq, pq.Len()-1)
}

func (pq *PriorityQueue) Pop() interface{} {
	old := *pq
	n := len(old)
	item := old[n-1]
	*pq = old[0 : n-1]
	heap.Fix(pq, pq.Len()-1)
	return item
}

func Merge(w LineWriter, readers ...LineReader) error {
	pq := make(PriorityQueue, 0, len(readers))
	heap.Init(&pq)
	for _, reader := range readers {
		line, err := reader.ReadLine()
		if err == io.EOF && len(line) == 0 {
			continue
		}
		heap.Push(&pq, &Item{value: line, reader: reader})
	}
	// outputFile, err := os.Create("/home/korshunovi/shad-go/engineerharin/output.txt")
	// if err != nil {
	//   return fmt.Errorf("ошибка при создании файла вывода: %v", err)
	// }
	for pq.Len() > 0 {
		item := heap.Pop(&pq).(*Item)
		err := w.Write(item.value)
		if err != nil {
			return err
		}
		nextElement, err := item.reader.ReadLine()
		if err == io.EOF && len(nextElement) == 0 {
			continue
		}
		if err != nil && err != io.EOF {
			return err
		}
		heap.Push(&pq, &Item{value: nextElement, reader: item.reader})
	}

	return nil
}

func Sort(w io.Writer, in ...string) error {
	tempFiles := make([]string, 0, len(in))

	for _, filePath := range in {
		file, err := os.Open(filePath)
		if err != nil {
			return err
		}
		reader := NewReader(file)
		var lines []string
		for {
			line, localErr := reader.ReadLine()
			if localErr == io.EOF && len(line) == 0 {
				break
			}
			if localErr != nil && localErr != io.EOF {
				file.Close()
				return err
			}
			lines = append(lines, line)
		}
		file.Close()
		sort.Strings(lines)

		file, err = os.OpenFile(filePath, os.O_RDWR|os.O_CREATE, 0755)
		if err != nil {
			return err
		}

		myWriter := NewWriter(file)
		for _, line := range lines {
			if err := myWriter.Write(line); err != nil {
				file.Close()
				return err
			}
		}
		file.Close()
		tempFiles = append(tempFiles, filePath)
	}
	lineReaders := make([]LineReader, 0, len(tempFiles))
	for _, tempfilePath := range tempFiles {
		tempFile, err := os.Open(tempfilePath)
		if err != nil {
			return err
		}
		lineReaders = append(lineReaders, NewReader(tempFile))
	}
	lineWriter := NewWriter(w)
	if err := Merge(lineWriter, lineReaders...); err != nil {
		return err
	}

	return nil
}
