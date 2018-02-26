package main

//I keep this file in my root so that I can easily test small stuff, I guess it can be useful for everyone idk

var x, y string = 4, 6 - 5 + 6 * 8 / 7
var x1, x2 = 5 && 7, 6 || "hello" , 7

var x [4]int

var (
  x []int
  x, y string = 5, 6
  z int = 0xff45abc
  y = 01237
  u [0]int
)

type float float64

type point struct {
  x, y float64
  x int
  y, y, y bool
  h, e [45]int
}

type (
  float float64

  point struct {
    x, y float64
    x int
    y, y, y bool
    u point
  }
)

func main(x, y int, x float64, y int, x,y,t string) int {

  var x, y string = 4, 6 - 5 + 6 * 8 / 7
  var x1, x2 = 5 && 7, 6 || "hello" , 7

  u[4] = 0
  u[1] += 3
  p.mem = 3

  if (4 == 4) {
    a++
    b--
  } else if 5 + 5 {
    //print (3+4)
  }
  else {
    y -= 5
  }

  type float int
  type (
    int int
    point struct {
      x, y int
    }
  )

  print (3+3) //this is a comment
  println (4+4)
  y = 7 + 7
  y += 6 && 8
  y -= 1
  y %= 34 / 5

  var (
    x, y string = 5, 6
    z int = 0xff45abc
    y = 01237
  )
}
