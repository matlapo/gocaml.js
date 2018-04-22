//~3
//~2
//~6
package main;

type a struct {
  a int
  x [4]int
  y [4][4]int
  z struct {
    x [4]int
    y [4][4]int
  }
}

func main() {
  var a a
  a.x[0] = 3
  var b []int
  a.z.y[2] = b
  a.z.x[1] = 3
  a.z.y[2][3] = 2
  a.a = 6
  println(a.z.x[1])
  println(a.z.y[2][3])
  println(a.a)
}
