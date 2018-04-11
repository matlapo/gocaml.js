//~0
//~1

package main

func f(arr [1]int) {
	arr[0] = 3
}

func main() {
	var a [1]int
	var b [1]int
	b = a
	a[0] = 1
	println(b[0])

	f(a)
	println(a[0])
}
