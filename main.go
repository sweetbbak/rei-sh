package main

import (
	"fmt"
	"log"
	"os"
	"os/user"

	"rei/repl"
)

func Run() error {
	user, err := user.Current()
	if err != nil {
		return err
	}

	fmt.Printf("Hello, %s! This is the rei lang repl!\n", user.Username)
	repl.Start(os.Stdin, os.Stdout)
	return nil
}

func main() {
	if err := Run(); err != nil {
		log.Fatal(err)
	}
}
