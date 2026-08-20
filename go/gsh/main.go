package main

import "fmt"

// TIP <p>To run your code, right-click the code and select <b>Run</b>.</p> <p>Alternatively, click
// the <icon src="AllIcons.Actions.Execute"/> icon in the gutter and select the <b>Run</b> menu item from here.</p>
func main() {
	// TIP <p>Press <shortcut actionId="ShowIntentionActions"/> when your caret is at the underlined text
	// to see how GoLand suggests fixing the warning.</p><p>Alternatively, if available, click the lightbulb to view possible fixes.</p>
	s := "gopher"
	fmt.Printf("Hello and welcome, %s!\n", s)

	for i := 1; i <= 5; i++ {
		// TIP <p>To start your debugging session, right-click your code in the editor and select the Debug option.</p> <p>We have set one <icon src="AllIcons.Debugger.Db_set_breakpoint"/> breakpoint
		// for you, but you can always add more by pressing <shortcut actionId="ToggleLineBreakpoint"/>.</p>
		fmt.Println("i =", 100/i)
	}
}

/*
Fanout function; generate a channel which forwards all messages to both channels.
blocks in the process, incidentally.
when the returned channel is closed, both supplied channels are then closed.
*/
func fanOut(left, right chan string) chan string {
	out := make(chan string)
	go func() {
		for s := range out {
			left <- s
			right <- s
		}
		close(left)
		close(right)
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
