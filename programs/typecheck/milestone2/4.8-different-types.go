// Typecheck error: cannot append an element with a different type from the array

package main

func main() {
  var a []int
  _ = append(a, 1.23)
}
