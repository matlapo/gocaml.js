//~There are 9592 prime numbers between 2 and 100000

package main

func confirmPrime(prime int) bool {
	for i:=prime - 1; i > 1; i-- {
		if prime % i == 0 {
			return false
		}
	}
	return true
}

func main() {
	var number_of_primes = 0
	var limit = 100000
	for i:=2; i <= limit; i++ {
		if (confirmPrime(i)) {
			number_of_primes++
		}
	}
	println("There are", number_of_primes, "prime numbers between 2 and", limit)
}
