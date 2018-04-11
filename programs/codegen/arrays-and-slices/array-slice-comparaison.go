//~true
//~true
package main

type ot struct {
  x [2]int
  y []string
}

type st struct {
  x, y []int
  z [12]ot
}

var a [2]int
var b [2]int

func main() {
  a[0] = 0
  a[1] = 1
  b[0] = 0
  b[1] = 1

  print(a == b)

  var foo ot
  foo.x[0] = 0
  foo.x[1] = 1

  var bar st
  bar.z[0] = foo

  print(bar.z[0].x == a)

  var er []string
  er = append(er, "hello")
  
  foo.y = append(foo.y, "hello")
  bar.z[0] = foo

  print(bar.z[0].y == er)
}

