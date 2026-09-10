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
func TestLcs0(t *testing.T) {
	assertLCS(t, 0, []int{1, 3, 2}, []int{})
}
func TestLcs00(t *testing.T) {
	assertLCS(t, 0, []int{}, []int{})
}

func TestLcs1(t *testing.T) {
	assertLCS(t, 1, []int{1, 3, 2}, []int{3, 4, 5})
}

func TestLcslong(t *testing.T) {
	assertLCS(t, 3, []int{1, 3, 2, 3, 5, 7, 9}, []int{0, 1, 3, 5, 7})
}

func TestLcslong2(t *testing.T) {
	assertLCS(t, 0, []int{1, 3, 2, 3, 5, 7, 9}, []int{0, 41, 33, 15, 87})
}

func TestLcslong3(t *testing.T) {
	assertLCS(
		t, 6,
		[]int{0, 1, 2, 3, 4, 5, 6},
		[]int{0, 0, 0, 1, 2, 3, 0, 1, 2, 3, 4, 5},
	)
}

func assertLCS(t *testing.T, expected int, sequence1 []int, sequence2 []int) {
	assert.Equal(
		t, expected,
		lcs(sequence1, sequence2),
	)
}

func TestBcsCost(t *testing.T) {
	assertLcsConst(t, 5, []float32{1, 1, 1, 4})
}

func assertLcsConst(t *testing.T, expected float32, f []float32) {
	assert.Equal(
		t, expected, minCostBst(f), "expected bcs cost of %s", f)
}

func assertAdd(t *testing.T, input, expected string) {
	result := string(mealy(mealy_add_sigma, mealy_add_lambda, input))
	assert.Equal(
		t, expected, result,
		"expected mealy add of %s to be '%s'", input, result)
}

func TestAddZero(t *testing.T) {
	assertAdd(t, "00", "01")
}
