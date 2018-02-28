package main

// Random number generator code pseudocode taken from https://en.wikipedia.org/wiki/Permuted_congruential_generator
var state = 0x4d595df4d0f33173 // Or something seed-dependent
var multiplier = 6364136223846793005
var increment = 1442695040888963407 // Or an arbitrary odd constant
var randMax = 0x100000000

func rotr32(x int, r int) int {
	return x>>r | x<<(-r&31)
}

func rand32bit() int {
	x := state
	// count = x >> 59, since 59 = 64 - 5
	// This hack is needed to avoid a warning with the real Go compiler
	count := (x >> 30) >> 29

	state = x*multiplier + increment
	x ^= x >> 18                // 18 = (64 - 27)/2
	return rotr32(x>>27, count) // 27 = 32 - 5
}

func rand(min int, max int) {
	return int(float(min) + float(max-min)*float(rand32bit())/float(randMax))
}

// Computes a^d mod n
func powMod(a, d, n int) {
	if d == 0 {
		return 1
	}
	if d == 1 {
		return a
	}

	tmp := powMod(a, d/2, n)
	if d%2 == 0 {
		return tmp * tmp
	}
	return a * tmp * tmp
}

// Performs a Miller-Rabin primality test on n with k passes
// Pseucode from https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test#Computational_complexity
func isProbablyPrime(n int, k int) {
	d := n - 1
	r := 0
	for d%2 == 0 {
		d /= 2
		r++
	}

	for i := 0; i < k; i++ {
		a = rand(2, n-1)
		x := powMod(a, d, n)
		if x == 1 || x == n-1 {
			continue
		}

		for j := 0; j < r-1; j++ {
			x = x * x % n
			if x == 1 {
				return false
			}
			if x == n-1 {
				continue
			}
		}

		return false
	}

	return true
}

func main() {
	for i := 1000000000000; i < 1000000001000; i++ {
		if isProbablyPrime(i) {
			println(i, "Prime")
		} else {
			println(i)
		}
	}
}
