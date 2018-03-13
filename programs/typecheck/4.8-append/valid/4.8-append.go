package main

type point struct {
  x, y int
}

func main() {
  var a []int
  _ = append(a, 1)
  var b []point
  var pt point
  _ = append(b, pt)
}
