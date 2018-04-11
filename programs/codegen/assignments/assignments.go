//~[]
//~3
//~2
//~6
package main;

type a struct {
  a int
  x []int
  y [][]int
  z struct {
    x [6]int
    y [][]int
  }
}

func main() {
  var a a
  a.x[0] = 3
  var b []int
  a.y[2] = b
  a.z.x[1] = 3
  a.z.y[2][3] = 2
  a.a = 6
  print(a.y[2])
  print(a.z.x[1])
  print(a.z.y[2][3])
  print(a.a)
}
