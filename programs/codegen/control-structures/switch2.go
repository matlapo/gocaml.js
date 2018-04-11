//~1
//~false

package main

func main() {
  var i int
  switch i := 1; i {
  case 1, 0:
    println(i)
    var i bool
    println(i)
    break
  case 2:
    break
  default:
    break
  }

  switch i := "ok"; {
  case true, false && false:
  }
}
