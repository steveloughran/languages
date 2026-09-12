package main

import (
  "slices"
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

func assertAdd(t *testing.T, inputsequence, expected []string) {
  slices.Reverse(inputsequence)
  sequence := mealy(mealy_add_sigma, mealy_add_lambda, inputsequence)
  slices.Reverse(expected)
	assert.Equal(
		t, expected, sequence,
		"expected mealy add of %s", inputsequence)
}

func TestAddZero(t *testing.T) {
	assertAdd(t, []string{"0","0"}, []string{"0","1"})
}

func TestAdd5to6(t *testing.T) {
	assertAdd(t, []string{"0", "1","0","1"}, []string{"0","1","1","0"})
}


func TestReverseList(t *testing.T) {
  assert.Equal(t, []string{"0","1","1","0", "0"}, reverseList([]string{"0","0","1", "1", "0"}))
}


func TestReverseList0(t *testing.T) {
  assert.Equal(t, []string{}, reverseList([]string{}))
}


func TestMooreInvert(t *testing.T) {
  inputsequence := []string{"0","1","1"}
  sequence := moore(moore_reverse_sigma, moore_reverse_lambda, inputsequence)
	assert.Equal(t, []string{"0","1","0", "0"}, sequence,
		"expected mealy add of %s", inputsequence)
}

func TestAccept(t *testing.T) {
  assertDfaAccept(t, []string{"1","1"})
  assertDfaAccept(t, []string{"0","0","0","0","0"})
}

func TestReject(t *testing.T) {
  assertDfaRejected(t, []string{""})
  assertDfaRejected(t, []string{"1","0"})
  assertDfaRejected(t, []string{"0","0","1","0","0"})
}

func assertDfaAccept(t *testing.T, input []string) {
  assert.True(t, dfa(zeros_or_ones, input, []int{2, 3}),
    "Did not accept %s", input)
}
func assertDfaRejected(t *testing.T, input []string) {
  assert.False(t, dfa(zeros_or_ones, input, []int{2, 3}),
    "Did not reject %s", input)
}
func TestNFAAccept(t *testing.T) {
  assertNfaAccept(t, []string{"0"})
  assertNfaAccept(t, []string{"0","0","0","0","0"})
}

func TestNFAReject(t *testing.T) {
  assertNfaReject(t, []string{"1"})
}


func TestNFAReject2(t *testing.T) {
  assertNfaReject(t, []string{"0","1","0","0","0"})
}


func assertNfaAccept(t *testing.T, input []string) {
  assert.True(t, nfa(at_least_one_zero, input, []int{2}),
    "Did not accept %s", input)
}
func assertNfaReject(t *testing.T, input []string) {
  assert.False(t, nfa(at_least_one_zero, input, []int{2}),
    "Did not reject %s", input)
}