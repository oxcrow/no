package core

import "os"

func ReadFile(filePath string) string {
	bytes, e := os.ReadFile(filePath)
	if e != nil {
		Die([]string{
			Duck("Unable to read file from system storage drive."),
			"",
			Info("I could not read \"" + filePath + "\", for some reason."),
			Info("I don't know why. Sorry."),
			"",
			Hint("Are you sure the path is correct?"),
		})
	}
	return string(bytes)
}
