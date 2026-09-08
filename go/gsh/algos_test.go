package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func single(w int) int {
	return w
}
func double(w int) int {
	return 2 * w
}

func TestKnapSingleton(t *testing.T) {
	assert.Equal(
		t, 4,
		knapsack([]int{1, 2}, single, 3),
	)
}

func TestKnapDouble(t *testing.T) {
	assert.Equal(
		t, 4,
		knapsack([]int{1, 2, 3, 4, 12}, double, 5),
	)
}
