package main

func confirmPrime(prime int) bool {
	for i:=prime - 1; i > 1; i-- {
		if float64(prime) / float64(i) ==  float64(prime / i) {
			return false
		}
	}
	return true
}

var maybePrimes [26]int

func main() {
	maybePrimes[0] = 964827
	maybePrimes[1] = 964829
	maybePrimes[2] = 964861
	maybePrimes[3] = 964871
	maybePrimes[4] = 964879
	maybePrimes[5] = 964884
	maybePrimes[6] = 964889
	maybePrimes[7] = 964897
	maybePrimes[8] = 964913
	maybePrimes[9] = 964927
	maybePrimes[10] = 964933
	maybePrimes[11] = 964932
	maybePrimes[12] = 964967
	maybePrimes[13] = 964969
	maybePrimes[14] = 964973
	maybePrimes[15] = 964984
	maybePrimes[16] = 965023
	maybePrimes[17] = 965047
	maybePrimes[18] = 965059
	maybePrimes[19] = 965087
	maybePrimes[20] = 965086
	maybePrimes[21] = 965101
	maybePrimes[22] = 965113
	maybePrimes[23] = 965117
	maybePrimes[24] = 965131
	maybePrimes[25] = 965147
	for i:=0; i < 24; i++ {
		if (confirmPrime(maybePrimes[i])) {
			println("Number ", maybePrimes[i], " is prime.")
		} else {
			println("Number ", maybePrimes[i], " is NOT prime.")
		}
	}
}
