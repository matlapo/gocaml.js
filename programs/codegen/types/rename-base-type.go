//~true
package main

func main() {
  type int float64
  var a int = int(5.4)
  println(a == a)
}
