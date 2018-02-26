package main

func factorial(n int) int {
	if n <= 1 {
		return 1
	}
	return n * factorial(n-1)
}

func main() {
	for i := 0; i < 15; i++ {
		println(factorial(i))
	}
}
