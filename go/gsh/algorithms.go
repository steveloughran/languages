package main

import (
  "fmt"
  "slices"
)

func knapsack(inputs []int, weigher func(int) int, limit int) int {

	weights := make([]bool, limit+1)

	// there's always *something* at the bottom
	weights[0] = true

	for _, x := range inputs {
		weight := weigher(x)
		fmt.Printf("Input %d weighs %d\n", x, weight)
		for j := limit; j >= 0; j-- {
			updated := j + weight
			if weights[j] && updated <= limit {
				fmt.Printf("Updating weight %d\n", updated)
				weights[updated] = true
			}
		}
	}
	// now find the highest match
	fmt.Println("looking for match")
	highest := limit
	for ; !weights[highest]; highest-- {
	}
	return highest
}

func intmax(a, b int) int {
	if a <= b {
		return b
	} else {
		return a
	}
}

// lcs longest common substring measurement
func lcs(S, T []int) int {
	m := len(T)
	// the dynamic programming arrays are one larger than the
	// second input sequence so that there is no need for special handling
	// of element 0
	L := make([]int, m+1)
	L1 := make([]int, m+1)
	for _, Si := range S {
		L1[0] = 0
		for j, Tj := range T {
			prev := j
			current := j + 1
			if Si == Tj {
				L1[current] = L[prev] + 1
			} else {
				L1[current] = intmax(L[current], L1[prev])
			}
		}
		// switch arrays for the next iteration
		L = L1
	}
	// the highest found value
	return L[m]
}

func minCostBst(f []float32) float32 {
	n := len(f)
	dim := n + 2
	C := make([][]float32, dim)
	for i := range dim {
		C[i] = make([]float32, dim)
	}

	for d := 2; d <= n+1; d++ {
		for i := 0; i <= n-d+1; i++ {
			j := i + d
			C[i][j] = 1e6
			for m := i + 1; m <= j-1; m++ {
				combined := C[i][m] + C[m][j]
				if combined <= C[i][j] {
					C[i][j] = combined
				}
			}
			for m := i + 1; m <= j-1; m++ {
				C[i][j] = C[i][j] + f[m-1]
			}
		}
	}

	return C[0][n]
}

func mealy(
  sigma func(int, string) int,
  lambda func(int, string) string,
  input []string) []string {

  state := 1
  //var in string
  output := make([]string, 0)
  for _, in:= range input {
    newstate := sigma(state, in)

    var out string = lambda(state, in)
    fmt.Printf("[%d] %s/%s ==> [%d]\n", state, in, out, newstate)
    state = newstate
    if newstate == 0 {
      return append(output, fmt.Sprintf("failed to recognise %s", in))
    }
    output = append(output, out)
  }
  return output
}

func mealy_add_sigma(state int, in string) int {
  switch state {
  case 1:
    switch in {
    case "0":
      return 2
    case "1":
      return 1
    default:
      return 0
    }
  case 2:
    switch in {
    case "0":
      return 2
    case "1":
      return 2
    default:
      return 0
    }
  default:
    return 0
  }
}

func mealy_add_lambda(state int, in string) string {
  switch state {
  case 1:
    switch in {
    case "0":
      return "1"
    case "1":
      return "0"
    default:
      return ""
    }
  case 2:
    switch in {
    case "0":
      return "0"
    case "1":
      return "1"
    default:
      return ""
    }
  default:
    return ""
  }
}

func reverseList(input []string) []string {
  return recursiveReverse(input)
}

func recursiveReverse(input []string) []string {
  if len(input) <= 1 {
    return input
  }
  // split to head and tail
  reversedTail := recursiveReverse(input[1:])
  return append(reversedTail, input[0])
}

/*
Moore machine

M = <Q, E, delta, sigma, lambda, q0>
  here lambda: Q -> delta only (the input symbol is not involved)
 */

func moore(
  sigma func(int, string) int,
  lambda func(int) string,
  input []string) []string {

  state := 1
  //var in string
  output := make([]string, 1, len(input))

  initial := lambda(1)
  fmt.Printf("[%d/%s]\n", state, initial)
  output[0] = initial
  for _, in:= range input {
    // enter new state
    newstate := sigma(state, in)

    // calculate output of new state
    var out = lambda(newstate)
    fmt.Printf("[%d] %s ==> [%d/%s]\n", state, in, newstate, out)
    state = newstate
    if newstate == 0 {
      return append(output, fmt.Sprintf("failed to recognise %s", in))
    }
    output = append(output, out)
  }
  return output
}

func moore_reverse_sigma(state int, in string) int {
  switch state {
  case 1:
    switch in {
    case "0":
      return 2
    case "1":
      return 1
    default:
      return 0
    }
  case 2:
    switch in {
    case "0":
      return 2
    case "1":
      return 1
    default:
      return 0
    }
  default:
    return 0
  }
}

func moore_reverse_lambda(state int) string {
  switch state {
  case 1:
      return "0"
  case 2:
      return "1"
  default:
    return "unknown"
  }
}

func dfa(
  sigma func(int, string) int,
  input []string,
  final []int) bool {

  fmt.Printf("\nDFA processing %s\n", input)
  state := 1
  for _, in:= range input {
    newstate := sigma(state, in)
    fmt.Printf("[%d] %s ==> [%d]\n", state, in, newstate)
    state = newstate
    if newstate == 0 {
      fmt.Printf("failed to recognise %s\n", in)
      return false;
    }
  }
  // now look for the state in the final set.
  if slices.Contains(final, state) {
    fmt.Println("Accepted")
    return true
  } else {
    fmt.Println("Rejected")
    return false
  }
}

func zeros_or_ones(state int, in string) int {
  switch state {
  case 1:
    switch in {
    case "0":
      return 2
    case "1":
      return 3
    default:
      return 0
    }
  case 2:
    switch in {
    case "0":
      return 2
    case "1":
      return 4
    default:
      return 0
    }
  case 3:
    switch in {
    case "0":
      return 4
    case "1":
      return 3
    default:
      return 0
    }
    case 4:
     return 4
  default:
    return 0
  }
}