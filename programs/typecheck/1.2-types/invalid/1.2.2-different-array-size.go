package main;

func main() {
  var a [32]int
  var b [31]int
  b = a // Should fail since both array do not have the same size
}
