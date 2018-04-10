//~0
//~0
//~
//~[0 0 0]
//~[]

package main

func main() {
	var a int
	var b float64
	var c string
	var d [3]int
	var e []int
	println(a)
	println(b) // Due to the go formatting rules, this will print 0 and not 0.0
	println(c)
	println(d)
	println(e)
}
