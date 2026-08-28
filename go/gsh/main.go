package main

import (
	"fmt"
	"os"
)

import "golang.org/x/tour/tree"

func main() {
	args := os.Args
	if len(args) > 1 {
		names := args[1:]
		processAll(names)
	} else {
		fmt.Printf("usage %s [names]\n", args[0]) // program path as invoked
	}

}

// Walk walks the tree t sending all values
// from the tree to the channel ch.
func Walk(t *tree.Tree, ch chan int) {
	if t == nil {
		return
	}
	Walk(t.Left, ch)
	ch <- t.Value
	Walk(t.Right, ch)
}

func BoundedWalk(t *tree.Tree, ch chan int) {
	Walk(t, ch)
	close(ch)
}

func Elements(t *tree.Tree) []int {
	var ch = make(chan int)
	go BoundedWalk(t, ch)
	elts := make([]int, 10)
	for v := range ch {
		elts = append(elts, v)
	}
	return elts
}

// SameByArrays determines whether the trees
// t1 and t2 contain the same values.
func SameByArrays(t1, t2 *tree.Tree) bool {
	// do the walks
	elts1 := Elements(t1)
	elts2 := Elements(t2)
	if len(elts1) != len(elts2) {
		return false
	}
	// now compare
	for i, v := range elts1 {
		if elts2[i] != v {
			return false
		}
	}
	return true
}

func SameByChannel(t1, t2 *tree.Tree) bool {
	ch1 := make(chan int)
	ch2 := make(chan int)
	go BoundedWalk(t1, ch1)
	go BoundedWalk(t2, ch2)
	index := 0
	for next := range ch1 {
		index += 1
		tree2, hasData := <-ch2
		if !hasData {
			fmt.Printf("[%d] t1: %d t2: EOF\n", index, next)
			return false
		}
		if next != tree2 {
			fmt.Printf("[%d] t1: %d t2: %d\n", index, next, tree2)
			return false
		}
	}
	// at this point, if there is any data in ch2 then there's a mismatch,
	// but all is
	tree2, hasData := <-ch2
	if hasData {
		fmt.Printf("[%d] t1: EOF t2: %d\n", index+1, tree2)
		return false

	}
	return true
}

// Same determines whether the trees
// t1 and t2 contain the same values.
func Same(t1, t2 *tree.Tree) bool {
	return SameByChannel(t1, t2)
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
