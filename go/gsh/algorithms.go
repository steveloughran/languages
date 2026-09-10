package main

import (
	"fmt"
  "unicode/utf8"
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

func mealy(sigma func(int, int) int, lambda func(int, int) int, input string) []byte {
  state := 1
  var in int
  output := make([]string, len(input))
  for in = range input {
    newstate := sigma(state, in)

    out := lambda(state, in)
    fmt.Printf("[%d] %c/%c ==> [%d]", state, in, out, newstate)
    state = newstate
    if newstate == 0 {
      return fmt.Sprintf("failed to recognise %d", in)
    }
    output = output.append(utf8.AppendRune(output, out)
  }
  return output
}

func mealy_add_sigma(state int, in rune) int {
  switch state {
  case 1:
    switch in {
    case '0':
      return 2
    case '1':
      return 1
    default:
      return 0
    }
  case 2:
    switch in {
    case '0':
      return 2
    case '1':
      return 2
    default:
      return 0
    }
  default:
    return 0
  }
}

func mealy_add_lambda(state int, in rune) rune {
  switch state {
  case 1:
    switch in {
    case '0':
      return '1'
    case '1':
      return '0'
    default:
      return ' '
    }
  case 2:
    switch in {
    case '0':
      return ' '
    case '1':
      return ' '
    default:
      return ' '
    }
  default:
    return ' '
  }
}