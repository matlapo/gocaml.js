package main

func confirmPrime(prime int) bool {
	for i:=prime - 1; i > 1; i-- {
		if float64(prime) / float64(i) ==  float64(prime / i) {
			return false
		}
	}
	return true
}

var maybePrimes []int

func main() {
	maybePrimes = append(maybePrimes, 964829)
	maybePrimes = append(maybePrimes, 964861)
	maybePrimes = append(maybePrimes, 964871)
	maybePrimes = append(maybePrimes, 964879)
	maybePrimes = append(maybePrimes, 964884)
	maybePrimes = append(maybePrimes, 964889)
	maybePrimes = append(maybePrimes, 964897)
	maybePrimes = append(maybePrimes, 964913)
	maybePrimes = append(maybePrimes, 964927)
	maybePrimes = append(maybePrimes, 964933)
	maybePrimes = append(maybePrimes, 964932)
	maybePrimes = append(maybePrimes, 964967)
	maybePrimes = append(maybePrimes, 964969)
	maybePrimes = append(maybePrimes, 964973)
	maybePrimes = append(maybePrimes, 964984)
	maybePrimes = append(maybePrimes, 965023)
	maybePrimes = append(maybePrimes, 965047)
	maybePrimes = append(maybePrimes, 965059)
	maybePrimes = append(maybePrimes, 965087)
	maybePrimes = append(maybePrimes, 965086)
	maybePrimes = append(maybePrimes, 965101)
	maybePrimes = append(maybePrimes, 965113)
	maybePrimes = append(maybePrimes, 965117)
	maybePrimes = append(maybePrimes, 965131)
	maybePrimes = append(maybePrimes, 965147)
	for i:=0; i < 24; i++ {
		if (confirmPrime(maybePrimes[i])) {
			println("Number ", maybePrimes[i], " is prime.")
		} else {
			println("Number ", maybePrimes[i], " is NOT prime.")
		}
	}
}
