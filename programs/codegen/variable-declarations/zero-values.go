//~0
//~0
//~
//~[0 0 0]
//~[]

package main

type S struct {
	field1 int
	field2 [3]string
}

func main() {
	var a int
	var b float64
	var c string
	var d [3]int
	var e []int
	var f S
	println(a)
	println(b) // Due to the go formatting rules, this will print 0 and not 0.0
	println(c)
	println(d)
	println(e)
	println(f)
}
