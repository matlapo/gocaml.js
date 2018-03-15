// Typecheck error: cannot index a type that is not an array or a slice

package main

func main() {
  var a int
  var b = a[0]
}
