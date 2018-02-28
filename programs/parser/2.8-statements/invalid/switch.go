// A switch statement cannot contain two default clauses
package main

func main() {
	x := 5
	switch x {
	default:
		print(3)
	default:
		print(4)
	}
}
