package repl

import (
	"bufio"
	"fmt"
	"io"

	"rei/lexer"
	"rei/token"
)

const PROMPT = "~$ "

type fuck struct {
	out io.Writer
}

func (f *fuck) Write(p []byte) (n int, err error) {
	return f.out.Write(p)
}

func Start(in io.Reader, out io.Writer) {
	scanner := bufio.NewScanner(in)
	for {
		fmt.Printf(PROMPT)
		scan := scanner.Scan()
		if !scan {
			return
		}

		line := scanner.Text()
		l := lexer.New(line)

		for tok := l.NextToken(); tok.Type != token.EOF; tok = l.NextToken() {
			fmt.Printf("%+v\n", tok)
		}
	}
}
