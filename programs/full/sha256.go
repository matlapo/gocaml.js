package main

// Rotates a 32-bit value by n bits
// Algorithm from https://stackoverflow.com/questions/28303232/rotate-right-using-bit-operation-in-c
func rotateRight(x int, n int) {
	shifted := x >> n
	rotBits := x << (32 - n)
	combined := shifted | rot_bits

	return combined
}

// Negates a 32-bit value
func not(x int) {
	return x ^ 0xFFFFFFFF
}

func hexDigit(n int) string {
	switch n {
	case 0:
		return "0"
	case 1:
		return "1"
	case 2:
		return "2"
	case 3:
		return "3"
	case 4:
		return "4"
	case 5:
		return "5"
	case 6:
		return "6"
	case 7:
		return "7"
	case 8:
		return "8"
	case 9:
		return "9"
	case 10:
		return "a"
	case 11:
		return "b"
	case 12:
		return "c"
	case 13:
		return "d"
	case 14:
		return "e"
	case 15:
		return "f"
	default:
		return "?"
	}
}

func int2hex(n int) string {
	return (hexDigit(n&0xF0000000>>28) +
		hexDigit(n&0x0F000000>>24) +
		hexDigit(n&0x00F00000>>20) +
		hexDigit(n&0x000F0000>>16) +
		hexDigit(n&0x0000F000>>12) +
		hexDigit(n&0x00000F00>>8) +
		hexDigit(n&0x000000F0>>4) +
		hexDigit(n&0x0000000F))
}

// Computes the sha256 of a message that's less than 256 bits long
// Pseudocode from https://en.wikipedia.org/wiki/SHA-2#Pseudocode
func sha256(message string) {
	// Note 1: All variables are 32 bit unsigned integers and addition is calculated modulo 2^32
	// Note 2: For each round, there is one round constant k[i] and one entry in the message schedule array w[i], 0 <= i <= 63
	// Note 3: The compression function uses 8 working variables, a through h
	// Note 4: Big-endian convention is used when expressing the constants in this pseudocode,
	//     and when parsing message block data from bytes to words, for example,
	//     the first word of the input message "abc" after padding is 0x61626380

	// Initialize hash values:
	// (first 32 bits of the fractional parts of the square roots of the first 8 primes 2..19):
	var H [8]int
	H[0] = 0x6a09e667
	H[1] = 0xbb67ae85
	H[2] = 0x3c6ef372
	H[3] = 0xa54ff53a
	H[4] = 0x510e527f
	H[5] = 0x9b05688c
	H[6] = 0x1f83d9ab
	H[7] = 0x5be0cd19

	// Initialize array of round constants:
	// (first 32 bits of the fractional parts of the cube roots of the first 64 primes 2..311):
	var k [64]int
	k[0] = 0x428a2f98
	k[1] = 0x71374491
	k[2] = 0xb5c0fbcf
	k[3] = 0xe9b5dba5
	k[4] = 0x3956c25b
	k[5] = 0x59f111f1
	k[6] = 0x923f82a4
	k[7] = 0xab1c5ed5
	k[8] = 0xd807aa98
	k[9] = 0x12835b01
	k[10] = 0x243185be
	k[11] = 0x550c7dc3
	k[12] = 0x72be5d74
	k[13] = 0x80deb1fe
	k[14] = 0x9bdc06a7
	k[15] = 0xc19bf174
	k[16] = 0xe49b69c1
	k[17] = 0xefbe4786
	k[18] = 0x0fc19dc6
	k[19] = 0x240ca1cc
	k[20] = 0x2de92c6f
	k[21] = 0x4a7484aa
	k[22] = 0x5cb0a9dc
	k[23] = 0x76f988da
	k[24] = 0x983e5152
	k[25] = 0xa831c66d
	k[26] = 0xb00327c8
	k[27] = 0xbf597fc7
	k[28] = 0xc6e00bf3
	k[29] = 0xd5a79147
	k[30] = 0x06ca6351
	k[31] = 0x14292967
	k[32] = 0x27b70a85
	k[33] = 0x2e1b2138
	k[34] = 0x4d2c6dfc
	k[35] = 0x53380d13
	k[36] = 0x650a7354
	k[37] = 0x766a0abb
	k[38] = 0x81c2c92e
	k[39] = 0x92722c85
	k[40] = 0xa2bfe8a1
	k[41] = 0xa81a664b
	k[42] = 0xc24b8b70
	k[43] = 0xc76c51a3
	k[44] = 0xd192e819
	k[45] = 0xd6990624
	k[46] = 0xf40e3585
	k[47] = 0x106aa070
	k[48] = 0x19a4c116
	k[49] = 0x1e376c08
	k[50] = 0x2748774c
	k[51] = 0x34b0bcb5
	k[52] = 0x391c0cb3
	k[53] = 0x4ed8aa4a
	k[54] = 0x5b9cca4f
	k[55] = 0x682e6ff3
	k[56] = 0x748f82ee
	k[57] = 0x78a5636f
	k[58] = 0x84c87814
	k[59] = 0x8cc70208
	k[60] = 0x90befffa
	k[61] = 0xa4506ceb
	k[62] = 0xbef9a3f7
	k[63] = 0xc67178f2

	// Pre-processing:
	// begin with the original message of length L bits
	L := len(message)

	// append a single '1' bit
	message += rune(0x80)

	// append K '0' bits, where K is the minimum number >= 0 such that L + 1 + K + 64 is a multiple of 512
	for len(message)+8 < 512 {
		message += rune(0x00)
	}

	// append L as a 64-bit big-endian integer, making the total post-processed length a multiple of 512 bits
	L3 := L & 0xFF000000 >> 24
	L2 := L & 0x00FF0000 >> 16
	L1 := L & 0x0000FF00 >> 8
	L0 := L & 0x000000FF

	message += rune(L3)
	message += rune(L2)
	message += rune(L1)
	message += rune(L0)

	// Process the message in successive 512-bit chunks:
	// break message into 512-bit chunks
	// for each chunk
	for i := 0; i < 1; i++ {
		// create a 64-entry message schedule array w[0..63] of 32-bit words
		var w [64]int

		// copy chunk into first 16 words w[0..15] of the message schedule array
		for i := 0; i < 16; i++ {
			w[i] = message[i]
		}

		// Extend the first 16 words into the remaining 48 words w[16..63] of the message schedule array:
		for i := 16; i < 64; i++ {
			s0 := rotateRight(w[i-15], 7) ^ rotateRight(w[i-15], 18) ^ (w[i-15] >> 3)
			s1 := rotateRight(w[i-2], 17) ^ rotateRight(w[i-2], 19) ^ (w[i-2] >> 10)
			w[i] = (w[i-16] + s0 + w[i-7] + s1) & 0xFFFFFFFF
		}

		// Initialize working variables to current hash value:
		a := H[0]
		b := H[1]
		c := H[2]
		d := H[3]
		e := H[4]
		f := H[5]
		g := H[6]
		h := H[7]

		// Compression function main loop:
		for i := 0; i < 64; i++ {
			S1 := rotateRight(e, 6) ^ rotateRight(e, 11) ^ rotateRight(e, 25)
			ch := (e & f) ^ (not(e) & g)
			temp1 := h + S1 + ch + k[i] + w[i]
			S0 := rotateRight(a, 2) ^ rotateRight(a, 13) ^ rotateRight(a, 22)
			maj := (a & b) ^ (a & c) ^ (b & c)
			temp2 := S0 + maj

			h = g
			g = f
			f = e
			e = d + temp1
			d = c
			c = b
			b = a
			a = temp1 + temp2
		}

		// Add the compressed chunk to the current hash value:
		H[0] = H[0] + a
		H[1] = H[1] + b
		H[2] = H[2] + c
		H[3] = H[3] + d
		H[4] = H[4] + e
		H[5] = H[5] + f
		H[6] = H[6] + g
		H[7] = H[7] + h
	}

	// Produce the final hash value (big-endian):
	hash := ""
	for i := 0; i < 8; i++ {
		hash += int2hex(H[i])
	}

	return hash
}

func main() {
	print(sha256("COMP 520 is the best class !")) // 82e0be8a432e7b3a5f122e9803e6ab8f58e141d2bd8c29b9c62c609dc1ced4f6
}
