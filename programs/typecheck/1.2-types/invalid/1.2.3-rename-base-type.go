package main;

func main() {
  type int float64
  var a int = 5.4 // Should fail because float64 and int are not strictly the same type
  a = a
}
