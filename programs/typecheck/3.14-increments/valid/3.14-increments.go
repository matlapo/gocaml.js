package main

type ok struct {
  x int
}

func main() {
  var a int
  var b float64
  var c rune
  a++
  a--
  b++
  b--
  c++
  c--
  var d ok
  d.x++
}
