package main

type point struct {
  x, y int
}

type vector struct {
  a point
  b point
}

type vector_implicit struct {
  a struct {
    x, y int
  }
  b struct {
    x, y int
  }
}

func main() {
  var v1 vector
  var v2 vector
  v1.a.x = 10
  v2.b.y = v1.a.y
  v1.a = v2.a
  var v3 vector_implicit
  v3.a.x = v3.b.y
}
