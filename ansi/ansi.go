package ansi

import (
	"bytes"
)

const (
	CLEAR = "\x1b[0m"
	RED   = "\x1b[31m"
	GREEN = "\x1b[32m"
	BLUE  = "\x1b[33m"
)

func ColorizeErrorMessage(format string) string {
	end := len(format)
	var buf bytes.Buffer

	for i := 0; i < end; {
		lasti := i
		for i < end && format[i] != '%' {
			i++
		}

		if i > lasti {
			buf.WriteString(format[lasti:i])
		}

		buf.WriteString("\x1b[31m")

		if i >= end {
			buf.WriteString("\x1b[0m")
			// done processing format string
			break
		}

		ch := format[i]
		for i < end && !('a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z') {
			i++
		}

		buf.WriteString("\x1b[0m")
		// Process one verb
		i++
	}

	return buf.String()
}
