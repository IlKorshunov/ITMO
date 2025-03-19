package fileleak

import (
	"fmt"
	"os"
	"syscall"
)

type testingT interface {
	Errorf(msg string, args ...interface{})
	Cleanup(func())
}

type FileDescriptorInfo struct {
	Path  string
	Inode uint64
}

func ReadOpenFiles(t testingT) ([]FileDescriptorInfo, error) {
	var openFiles []FileDescriptorInfo
	dirPath := "/proc/self/fd"
	dir, err := os.Open(dirPath)
	if err != nil {
		t.Errorf("Ошибка при открытии директории: %v", err)
		return nil, err
	}
	defer dir.Close()

	files, err := dir.Readdir(-1)
	if err != nil {
		t.Errorf("Ошибка при чтении директории: %v", err)
		return nil, err
	}

	for _, file := range files {
		linkPath := fmt.Sprintf("/proc/self/fd/%s", file.Name())
		targetPath, err := os.Readlink(linkPath)
		if err != nil {
			t.Errorf("Ошибка при чтении символической ссылки: %v", err)
			continue
		}

		fileStat, err := os.Lstat(linkPath)
		if err != nil {
			t.Errorf("Ошибка при получении информации о файле: %v", err)
			continue
		}

		stat, ok := fileStat.Sys().(*syscall.Stat_t)
		if !ok {
			t.Errorf("Ошибка при преобразовании информации о файле")
			continue
		}

		openFiles = append(openFiles, FileDescriptorInfo{
			Path:  targetPath,
			Inode: stat.Ino,
		})
	}

	return openFiles, nil
}

func VerifyNone(t testingT) {
	startFiles, err := ReadOpenFiles(t)
	if err != nil {
		t.Errorf("Не удалось получить список открытых файлов в начале теста: %v", err)
		return
	}

	t.Cleanup(func() {
		endFiles, err := ReadOpenFiles(t)
		if err != nil {
			t.Errorf("Не удалось получить список открытых файлов в конце теста: %v", err)
			return
		}

		for _, file := range endFiles {
			if !contains(startFiles, file) {
				t.Errorf("Обнаружена утечка файла")
			}
		}
	})
}

func contains(slice []FileDescriptorInfo, item FileDescriptorInfo) bool {
	for _, a := range slice {
		if a.Inode == item.Inode && a.Path == item.Path {
			return true
		}
	}
	return false
}
