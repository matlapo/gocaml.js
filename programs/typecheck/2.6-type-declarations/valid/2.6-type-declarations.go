package main;

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
