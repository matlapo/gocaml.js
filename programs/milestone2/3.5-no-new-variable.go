// Declaration error: a short declaration must have at least one new variable
// on its left-hand-side.

package main

func main() {
  var a int
  var b int
  a, b := 1, 2
}
