package build

import (
	"crypto/rand"
	"crypto/sha1"
	"encoding"
	"encoding/hex"
	"fmt"
	"path/filepath"
)

type ID [sha1.Size]byte

var (
	_ = encoding.TextMarshaler(ID{})
	_ = encoding.TextUnmarshaler(&ID{})
)

// преобразовывает массив байт в их представление в системе 16
func (id ID) String() string {
	return hex.EncodeToString(id[:])
}

// создает путь до данного id, состоящий из конкатенации 1 символа id и всего id 
func (id ID) Path() string {
	return filepath.Join(hex.EncodeToString(id[:1]), hex.EncodeToString(id[:]))
}

// принимает массив бинарных данных и возвращает их шестнадцатеричное представление
func (id ID) MarshalText() ([]byte, error) {
	return []byte(hex.EncodeToString(id[:])), nil
}

// принимает байтовый массив, представляющий шестнадцатеричные данные в виде текста,
// и конвертирует эти данные обратно в их бинарный формат
func (id *ID) UnmarshalText(b []byte) error {
	raw, err := hex.DecodeString(string(b))
	if err != nil {
		return err
	}

	if len(raw) != len(id) {
		return fmt.Errorf("invalid id size: %q", b)
	}

	copy(id[:], raw)
	return nil
}

func NewID() ID {
	var id ID
	_, err := rand.Read(id[:])
	if err != nil {
		panic(fmt.Sprintf("crypto/rand is unavailable: %v", err))
	}
	return id
}
