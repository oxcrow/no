package core

import "os"

func ReadFile(filePath string) string {
	bytes, e := os.ReadFile(filePath)
	if e != nil {
		panic("Unable to read file.")
	}
	return string(bytes)
}
