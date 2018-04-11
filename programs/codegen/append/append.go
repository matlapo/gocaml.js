//~1
//~10
//~0

package main

type point struct {
  x, y int
}

func main() {
  var a []int
  a = append(a, 1)
  var b []point
  var pt point
  pt.x = 10
  b = append(b, pt)
  println(a[0])
  println(b[0].x)
  print(b[0].y) // Test default value
}

