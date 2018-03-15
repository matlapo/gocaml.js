// Declaration error: the scope of a block should end when the block ends.

package main;

func main() {
  {
    var a int
  }
  a = a
}
