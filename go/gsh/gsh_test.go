package main

import (
	"testing"

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

func assertChannelClosed(t *testing.T, left chan string) {
	s, r := <-left
	assert.False(t, r, "channel is open and returned %s", s)
}

func TestLaunch(t *testing.T) {

	alice, stop := launch("alice", stdout())
	alice <- "hello"
	alice <- "alice"
	close(alice)
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
