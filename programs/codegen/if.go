//~a
//~b
//~3
//~1
//~2

package main

func main() {
  if true {
    println("a")
  }

  if false {
    println("oops")
  } else {
    println("b")
  }

  var a = 1;
  if a:= 3; 2 < a {
    println(a)
  }
  println(a)

  if a:= 2; a < 2 {
    println("nope")
  } else {
    println(a)
  }
}
