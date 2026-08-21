package main

import (
	"fmt"
	"os"
)

func main() {
	args := os.Args
	fmt.Println(args[0]) // program path as invoked
	if len(args) > 1 {
		names := args[1:]
		processAll(names)
	}

}

func processAll(names []string) {
	out := stdout()
	var commonInput chan string = nil
	var commonStop []chan bool
	// each arg is the name of an entity
	for _, v := range names {
		agent, stop := launch(v, out)
		agent <- "launched " + v
		commonInput = fanOut(agent, commonInput)
		commonStop = append(commonStop, stop)
		commonInput <- "new agent " + v
	}

	// at the end, so close them all.
	commonInput <- "closing"
	close(commonInput)

	for _, v := range commonStop {
		<-v
	}
}

/*
Fanout function; generate a channel which forwards all messages to both channels.
blocks in the process, incidentally.
when the returned channel is closed, both supplied channels are then closed.

special handling of right==nil, where the left is returned.
*/
func fanOut[Alpha any](left, right chan Alpha) chan Alpha {
	out := make(chan Alpha)
	go func() {
		for s := range out {
			left <- s
			if right != nil {
				right <- s
			}
		}
		close(left)
		if right != nil {
			close(right)
		}
	}()
	return out
}

/*
launch: name, printer channel

	returns
	  - channel to send messages to
	  - bool channel set to true then closed when stopped.
*/
func launch(name string, printer chan string) (chan string, chan bool) {
	input := make(chan string)
	stopper := make(chan bool)
	go func() {
		for s := range input {
			printer <- name + ": " + s
		}
		printer <- name + ": Stopped!"
		stopper <- true
		close(stopper)

	}()
	return input, stopper
}

func stdout() chan string {
	input := make(chan string)
	go func() {
		for s := range input {
			fmt.Println(s)
		}
	}()
	return input
}
