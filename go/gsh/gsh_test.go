package main

import (
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
)

func TestFanOut(t *testing.T) {
	left := make(chan string)
	right := make(chan string)
	out := fanOut(left, right)
	out <- "test"
	assert.Equal(t, "test", <-left)
	assert.Equal(t, "test", <-right)
	close(out)
	assertChannelClosed(t, left)
	assertChannelClosed(t, right)
}

func TestFanOut1(t *testing.T) {
	left := make(chan string)
	out := fanOut(left, nil)
	out <- "test"
	assert.Equal(t, "test", <-left)
	close(out)
	assertChannelClosed(t, left)
}

func assertChannelClosed[Alpha any](t *testing.T, ch <-chan Alpha) {
	t.Helper()
	select {
	case s, r := <-ch:
		assert.False(t, r, "channel is open and returned %s", s)
	case <-time.After(time.Second):
		assert.Fail(t, "no response from channel")
	}
}

func TestLaunch(t *testing.T) {

	alice, stop := launch("alice", stdout())
	alice <- "hello"
	alice <- "alice"
	close(alice)
	assert.True(t, <-stop)

}

func TestMulti(t *testing.T) {

	stdout := stdout()
	heather, stop := launch("heather", stdout)
	jim, stopJim := launch("jim", stdout)
	both := fanOut(heather, jim)

	both <- "hello"
	heather <- "I'm heather"
	jim <- "I'm jim"
	both <- "both of us"
	close(both)
	assert.True(t, <-stop)
	assert.True(t, <-stopJim)

}

func TestProcessOne(t *testing.T) {
	var names []string
	names = []string{"single"}
	processAll(names)
}

func TestProcessMany(t *testing.T) {
	var names []string
	names = []string{"gillian", "heather", "jim", "konrad"}
	processAll(names)
}
