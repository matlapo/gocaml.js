package main

func main() {
  var i int
  switch i := 0; i {
  case 1:
    var i int
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
