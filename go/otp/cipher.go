//go:build !solution

package otp

import (
	"io"
)

type myReader struct {
	read io.Reader
	prng io.Reader
}

type myWriter struct {
	write io.Writer
	prng  io.Reader
}

func NewReader(r io.Reader, prng io.Reader) io.Reader {
	return &myReader{read: r, prng: prng}
}

func NewWriter(w io.Writer, prng io.Reader) io.Writer {
	return &myWriter{write: w, prng: prng}
}

func (x *myReader) Read(p []byte) (n int, err error) {
	data := make([]byte, len(p))
	n, err = x.read.Read(data)
	key := make([]byte, n)
	_, _ = x.prng.Read(key)
	for i := 0; i < n; i++ {
		p[i] = key[i] ^ data[i]
	}
	return n, err
}

func (x *myWriter) Write(p []byte) (n int, err error) {
	key := make([]byte, len(p))
	data := make([]byte, len(p))
	_, _ = x.prng.Read(key)
	for i := range p {
		data[i] = p[i] ^ key[i]
	}
	n, err = x.write.Write(data)
	return n, err
}
