//~0

package main

func main() {
	var a [1]int
	var b [1]int
	b = a
	a[0] = 1
	println(b[0])
}
