// This program solves Project Euler problem #78 using dynamic programming
package main

var cache [][]int

func get(c [][]int, a int, b int) int {
	if len(c) > a && len(c[a]) > b {
		return c[a][b]
	}
	
	return -1
}

func set(c [][]int, a int, b int, value int) {
	for len(c) <= a {
		var tmp []int
		c = append(c, tmp)
	}

	for len(c[a]) <= b {
		var tmp []int
		c[a] = append(c[a], -1)
	}

	c[a][b] = value
}

// Computes the partition number of n
// Inspired by https://www.programminglogic.com/integer-partition-algorithm/
func partition(sum int, largestNumber int) {
	if sum < 0 {
		return 0
	}
	if sum == 0 {
		return 1
	}
	if largestNumber == 0 {
		return 0
	}

	cached := get(cache, sum, largestNumber)
	if cached != -1 {
		return cached
	}

	result := partition(sum, largestNumber-1) + partition(sum-largestNumber, largestNumber)
	set(cache, sum, largestNumber, result)
	return result
}

func main() {
	for i := 0; true; i++ {
		p := partition(i)
		if p%1000000 == 0 {
			println(i)
			println(p)
			break
		}
	}
}
