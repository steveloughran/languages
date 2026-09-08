package main

import "fmt"

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
