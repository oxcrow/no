package core

func CharAt(text string, index int) byte {
	if index >= len(text) {
		return ' '
	}
	return text[index]
}

func IsAlpha(c byte) bool {
	if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') {
		return true
	}
	return false
}

func IsDigit(c byte) bool {
	if c >= '0' && c <= '9' {
		return true
	}
	return false
}
